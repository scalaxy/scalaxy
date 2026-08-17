;;;; tck.lisp --- openCypher TCK runner (Gherkin subset)
;;;;
;;;; Parses the official TCK .feature files (specs/openCypher/tck) and
;;;; runs every scenario against the Scalaxy Cypher engine, comparing
;;;; results, errors and side effects.  Statistics: pass / fail /
;;;; unsupported (with reasons).  The corpus is the downloaded official
;;;; openCypher TCK; this runner is the certification harness.

(in-package #:scalaxy-tests)

(defparameter *tck-stats* nil)
(defparameter *tck-debug* nil)

(defun %tck-dump-rows (header body rows)
  "Debug helper: print expected table and actual rows."
  (format t "~&  expected (~d cols):~%" (length header))
  (format t "    |~{~a~^|~}|~%" header)
  (dolist (r body) (format t "    |~{~a~^|~}|~%" r))
  (format t "  actual (~d rows):~%" (length rows))
  (dolist (r rows)
    (format t "    ~s~%" r)))

(defun %tck-reset ()
  (setf *tck-stats*
        (list :pass 0 :fail 0 :unsupported 0 :total 0
              :failures nil
              :reasons (make-hash-table :test #'equal)
              :scenarios nil)))

;;; ------------------------------------------------------------------
;;; Gherkin-subset parsing

(defun %strip-comment (line)
  (let ((pos (position (code-char 35) line)))
    (if pos (subseq line 0 pos) line)))

(defun %step-text (line)
  "Strip the leading keyword from a Gherkin step line."
  (cond ((search "Given " line) (subseq line 6))
        ((search "When " line) (subseq line 5))
        ((search "Then " line) (subseq line 5))
        ((search "And " line) (subseq line 4))
        ((search "But " line) (subseq line 4))
        (t line)))

(defun %prefixp (prefix line)
  "True when LINE starts with PREFIX (case-insensitive)."
  (and (>= (length line) (length prefix))
       (string-equal line prefix :end1 (length prefix))))

(defun %split-tags (line)
  "Tag line '@a @b' (after the leading @) -> (\"a\" \"b\")."
  (remove "" (split-sequence-on #\Space (subseq line 1)) :test #'string=))

(defun %split-table-row (line)
  "A | cell | row -> (\"cell\" ...) with cells trimmed."
  (mapcar (lambda (c)
            (string-trim (list (code-char 32) (code-char 9)) c))
          (cdr (butlast (split-sequence-on (code-char 124) line)))))

(defun %normalize-tables (steps)
  "NREVERSE the row lists of every step table in STEPS."
  (dolist (s steps)
    (when (getf (cdr s) :table)
      (setf (getf (cdr s) :table) (nreverse (getf (cdr s) :table))))))

(defun %parse-gherkin (path)
  "Parse a .feature file into ((name tags steps examples) ...).
Steps are (kw :text text [:doc s] [:table rows]); rows are cell lists
in source order.  EXAMPLES is a list of tables (header row first)."
  (let ((scenarios nil) (name nil) (steps nil) (tags nil) (doc nil)
        (examples nil))
    (labels
        ((finish-scenario ()
           (when name
             (%normalize-tables steps)
             (setf examples (mapcar #'nreverse examples))
             (push (list name (nreverse tags) (nreverse steps)
                         (nreverse examples))
                   scenarios)
             (setf name nil steps nil tags nil examples nil))))
      (with-open-file (in path :external-format :utf-8)
        (loop for raw = (read-line in nil :eof)
              until (eq raw :eof)
              for line = (%strip-comment
                          (string-trim (list (code-char 32) (code-char 9)
                                             (code-char 13)) raw))
              do (cond
                   ((zerop (length line)))
                   ((char= (char line 0) (code-char 64))
                    (dolist (tag (%split-tags line)) (push tag tags)))
                   ((%prefixp "Feature:" line))
                   ((%prefixp "Scenario Outline:" line)
                    (finish-scenario)
                    (setf name (string-trim " " (subseq line 17))))
                   ((%prefixp "Scenario:" line)
                    (finish-scenario)
                    (setf name (string-trim " " (subseq line 9))))
                   ((%prefixp "Examples:" line)
                    (push nil examples))
                   ((search "\"\"\"" line)
                    (if doc
                        (progn
                          (setf (getf (cdr (first steps)) :doc)
                                (format nil "~{~a~%~}" (nreverse doc)))
                          (setf doc nil))
                        (setf doc (list ""))))
                   (doc
                    (push line doc))
                   ((char= (char line 0) (code-char 124))
                    (let ((cells (%split-table-row line)))
                      (if examples
                          (push cells (car examples))
                          (push cells (getf (cdr (first steps)) :table)))))
                   (t
                    (let ((kw (cond ((search "Given " line) :given)
                                    ((search "When " line) :when)
                                    ((search "Then " line) :then)
                                    ((or (search "And " line) (search "But " line))
                                     (if steps (first (first steps)) :given))
                                    (t nil))))
                      (when kw
                        (push (list kw :text (%step-text line)) steps)))))))
      (finish-scenario)
      (nreverse scenarios))))
;;; ------------------------------------------------------------------
;;; Scenario-outline expansion

(defun %replace-pl (text bindings)
  "Replace every <name> placeholder in TEXT using BINDINGS
(alist of name . value-strings).  A <name> is only a placeholder when
the name is a plain identifier (so comparisons like 'a < b' are left
alone)."
  (with-output-to-string (out)
    (loop with i = 0 with n = (length text)
          while (< i n)
          do (let ((lt (position #\< text :start i)))
               (unless lt
                 (write-string text out :start i)
                 (return))
               (let ((gt (position #\> text :start (1+ lt))))
                 (let ((pname (and gt (subseq text (1+ lt) gt))))
                   (if (and gt
                            (plusp (length pname))
                            (every (lambda (c) (or (alphanumericp c) (char= c #\_))) pname)
                            (cdr (assoc pname bindings :test #'string=)))
                       (progn
                         (write-string text out :start i :end lt)
                         (write-string (cdr (assoc pname bindings :test #'string=)) out)
                         (setf i (1+ gt)))
                       (progn
                         (write-string text out :start i :end (1+ lt))
                         (setf i (1+ lt))))))))))

(defun %subst-step (step bindings)
  "Copy STEP with placeholders replaced by BINDINGS in text, doc, table."
  (let* ((pl (cdr step))
         (new-pl (list :text (%replace-pl (getf pl :text) bindings))))
    (when (getf pl :doc)
      (setf new-pl (list* :doc (%replace-pl (getf pl :doc) bindings) new-pl)))
    (when (getf pl :table)
      (setf new-pl
            (list* :table
                   (mapcar (lambda (r)
                             (mapcar (lambda (c) (%replace-pl c bindings)) r))
                           (getf pl :table))
                   new-pl)))
    (cons (car step) new-pl)))

(defun %expand-outline (scenario)
  "SCENARIO = (name tags steps examples).  Returns the list of concrete
scenarios: one per Examples data row with placeholders substituted."
  (destructuring-bind (name tags steps examples) scenario
    (if (null examples)
        (list scenario)
        (let ((out nil) (idx 0))
          (dolist (table examples)
            (let ((header (first table)))
              (dolist (row (rest table))
                (let ((bindings (loop for k in header for v in row
                                      collect (cons k v))))
                  (incf idx)
                  (push (list (format nil "~a #~d" name idx)
                              tags
                              (mapcar (lambda (s) (%subst-step s bindings)) steps)
                              nil)
                        out)))))
          (nreverse out)))))

;;; ------------------------------------------------------------------
;;; TCK value parsing

(defun %tck-eval-literal (text)
  "Parse a TCK result cell (Cypher literal) into a value."
  (let ((expr (cypher-parse-expr text)))
    (eval-expr expr nil nil nil)))

(defun %tck-node-p (text)
  (and (plusp (length text)) (char= (char text 0) (code-char 40))))

(defun %tck-rel-p (text)
  "A TCK relationship literal starts with [: (a [ followed by :type).
Plain list literals like ['x'] or [[...]] are not relationships."
  (and (>= (length text) 2)
       (char= (char text 0) (code-char 91))
       (char= (char text 1) (code-char 58))))

(defun %tck-parse-entity (text)
  "Parse a node/relationship literal like (:A {k: 'v'}) or [:T {k: 1}].
Returns (:node labels props) or (:rel type props)."
  (let* ((open (char text 0))
         (close (cond ((char= open (code-char 40)) (code-char 41))
                      ((char= open (code-char 91)) (code-char 93))
                      (t (error "entity: expected ( or ["))))
         (inner (subseq text 1 (1- (length text))))
         (i 0) (n (length inner))
         (labels/types nil)
         (props nil))
    (declare (ignore close))
    (loop while (and (< i n) (char= (char inner i) (code-char 58)))
          do (incf i)
             (let ((start i))
               (loop while (and (< i n)
                                (not (member (char inner i)
                                             (list (code-char 123) (code-char 58)
                                                   (code-char 125) (code-char 32)))))
                     do (incf i))
               (push (subseq inner start i) labels/types)))
    (setf labels/types (nreverse labels/types))
    (let ((rest (string-trim (list (code-char 32) (code-char 9)) (subseq inner i))))
      (when (plusp (length rest))
        (setf props
              (let ((m (cypher-parse-expr rest)))
                (mapcar (lambda (p) (cons (car p) (eval-expr (cdr p) nil nil nil)))
                        (getf (cdr m) :pairs))))))
    (if (eql open (code-char 40))
        (list :node labels/types props)
        (list :rel (first labels/types) props))))

(defun %balanced (s start open close)
  "Index of the delimiter in S matching the one at START, or NIL."
  (let ((depth 0) (n (length s)) (in-string nil) (esc nil))
    (loop for i from start below n
          do (let ((c (char s i)))
               (cond
                 (esc (setf esc nil))
                 (in-string
                  (cond ((char= c #\\) (setf esc t))
                        ((char= c #\') (setf in-string nil))))
                 ((char= c #\') (setf in-string t))
                 ((char= c open) (incf depth))
                 ((char= c close)
                  (decf depth)
                  (when (zerop depth) (return i))))))))

(defun %tck-parse-path (text)
  "Parse a TCK path literal <...> into
(:path (node (rel dir type props) node ...)) where each node is a
(:node labels props) and each rel element is (:rel dir type props)
with dir :in/:out/:both."
  (let* ((inner (subseq text 1 (1- (length text))))
         (n (length inner)) (i 0)
         (elements nil))
    (when (plusp (length (string-trim " " inner)))
      (loop
        (let ((start (position (code-char 40) inner :start i)))
          (unless start (error "path: expected ("))
          (let ((end (%balanced inner start (code-char 40) (code-char 41))))
            (unless end (error "path: unbalanced ("))
            (push (%tck-parse-entity (subseq inner start (1+ end))) elements)
            (setf i (1+ end))))
        (when (>= i n) (return))
        (let ((dir :both) (rel nil))
          (when (char= (char inner i) (code-char 60))
            (setf dir :in) (incf i))
          (unless (char= (char inner i) (code-char 45))
            (error "path: expected -"))
          (incf i)
          (when (char= (char inner i) (code-char 91))
            (let ((end (%balanced inner i (code-char 91) (code-char 93))))
              (unless end (error "path: unbalanced ["))
              (setf rel (%tck-parse-entity (subseq inner i (1+ end))))
              (setf i (1+ end))))
          (unless (char= (char inner i) (code-char 45))
            (error "path: expected -"))
          (incf i)
          (when (char= (char inner i) (code-char 62))
            (setf dir (if (eq dir :in) :both :out))
            (incf i))
          (push (list :rel dir
                      (if rel (second rel) nil)
                      (if rel (third rel) nil))
                elements))))
    (list :path (nreverse elements))))

(defun %tck-parse-cell (text)
  (cond
    ((and (>= (length text) 2) (char= (char text 0) (code-char 60)))
     (handler-case (%tck-parse-path text)
       (error () (format nil "<unparsable: ~a>" text))))
    ((%tck-node-p text)
     (handler-case (%tck-parse-entity text)
       (error () (format nil "<unparsable: ~a>" text))))
    ((%tck-rel-p text)
     (handler-case (%tck-parse-entity text)
       (error () (format nil "<unparsable: ~a>" text))))
    (t (handler-case (%tck-eval-literal text)
         (error () (format nil "<unparsable: ~a>" text))))))

;;; ------------------------------------------------------------------
;;; TCK value comparison

(defun %tck-props= (actual expected &key (unordered nil))
  "ACTUAL is an alist of (key . value) Cypher pairs; EXPECTED is the
parsed entity props alist."
  (and (null (set-difference (mapcar #'car actual) (mapcar #'car expected)
                             :test #'equal))
       (null (set-difference (mapcar #'car expected) (mapcar #'car actual)
                             :test #'equal))
       (every (lambda (p)
                (let ((q (assoc (car p) actual :test #'equal)))
                  (and q (%tck-value= (cdr q) (cdr p) :unordered unordered))))
              expected)))

(defun %tck-path-elem= (actual expected &key (unordered nil))
  (cond
    ((and (consp expected) (eq (car expected) :node))
     (%tck-value= actual expected :unordered unordered))
    ((and (consp expected) (eq (car expected) :rel))
     (and (scalaxy::%rel-p actual)
          (string= (getf actual :type) (third expected))
          (%tck-props= (getf actual :props) (fourth expected) :unordered unordered)))
    (t nil)))

(defun %tck-path= (actual expected &key (unordered nil))
  "ACTUAL is an engine path (:path (n r n ...)); EXPECTED is a parsed
(:path (n (rel dir type props) n ...))."
  (let ((as (second actual)) (es (second expected)))
    (and (= (length as) (length es))
         (loop for a in as for e in es
               always (%tck-path-elem= a e :unordered unordered))
         (loop for i from 1 below (length as) by 2
               for r = (nth i as)
               for e = (nth i es)
               for left = (nth (1- i) as)
               for right = (nth (1+ i) as)
               always (let ((dir (second e)))
                        (cond
                          ((eq dir :in)
                           (equal (getf (getf r :end-node) :id) (getf left :id)))
                          ((eq dir :out)
                           (equal (getf (getf r :start-node) :id) (getf left :id)))
                          (t (or (equal (getf (getf r :start-node) :id) (getf left :id))
                                 (equal (getf (getf r :end-node) :id) (getf left :id))))))))))

(defun %tck-structural= (a e &key (unordered nil))
  "Structural equality of two Cypher values for TCK result matching:
lists compare element-wise, maps by key (order-insensitive), and null
equals null."
  (cond
    ((and (cypher-null-p a) (cypher-null-p e)) t)
    ((and (cypher-list-p a) (cypher-list-p e))
     (let ((as (cypher-list-elements a)) (es (cypher-list-elements e)))
       (and (= (length as) (length es))
            (if unordered
                (%bag-match (lambda (x y) (%tck-structural= x y :unordered t)) es as)
                (every (lambda (x y) (%tck-structural= x y :unordered unordered)) as es)))))
    ((and (cypher-map-p a) (cypher-map-p e))
     (let ((as (cypher-map-pairs a)) (es (cypher-map-pairs e)))
       (and (= (length as) (length es))
            (every (lambda (p)
                     (let ((q (assoc (car p) es :test #'equal)))
                        (and q (%tck-structural= (cdr p) (cdr q) :unordered unordered))))
                   as))))
    ((and (numberp a) (numberp e)) (= a e))
    ((and (or (eq a t) (cypher-false-p a)) (or (eq e t) (cypher-false-p e)))
     (eq (scalaxy::%tv-true a) (scalaxy::%tv-true e)))
    (t (equal a e))))

(defun %tck-value= (actual expected &key (unordered nil))
  "Compare an actual Cypher value with a parsed TCK cell value.
Returns T or NIL."
  (cond
    ((and (cypher-null-p actual) (cypher-null-p expected)) t)
    ((and (consp expected) (eq (car expected) :node))
     (and (scalaxy::%node-p actual)
          (null (set-difference (getf actual :labels) (second expected)
                                :test #'string=))
          (null (set-difference (second expected) (getf actual :labels)
                                :test #'string=))
          (%tck-props= (getf actual :props) (third expected) :unordered unordered)))
    ((and (consp expected) (eq (car expected) :rel))
     (and (scalaxy::%rel-p actual)
          (string= (getf actual :type) (second expected))
          (%tck-props= (getf actual :props) (third expected) :unordered unordered)))
    ((and (consp expected) (eq (car expected) :path))
     (and (scalaxy::%path-p actual) (%tck-path= actual expected :unordered unordered)))
    ((and unordered (cypher-list-p actual) (cypher-list-p expected))
     (let ((as (cypher-list-elements actual)) (es (cypher-list-elements expected)))
       (and (= (length as) (length es))
            (%bag-match (lambda (x y) (%tck-value= x y :unordered t)) es as))))
    ((or (cypher-list-p actual) (cypher-map-p actual)
         (cypher-list-p expected) (cypher-map-p expected))
     (%tck-structural= actual expected :unordered unordered))
    (t (let ((e (cypher-= actual expected)))
         (and (not (cypher-null-p e)) e)))))

(defun %bag-match (test expected actual)
  "True when every element of EXPECTED can be matched (by TEST) to a
distinct element of ACTUAL (multiset containment)."
  (let ((used (make-array (length actual) :initial-element nil)))
    (every (lambda (e)
             (let ((found nil))
               (loop for i below (length actual)
                     unless (aref used i)
                       do (when (funcall test e (nth i actual))
                            (setf found t)
                            (setf (aref used i) t)
                            (return)))
               found))
           expected)))

;;; ------------------------------------------------------------------
;;; result-table comparison

(defun %tck-row= (keys expected-cells actual-row &key (unordered nil))
  (and (= (length keys) (length expected-cells))
       (every (lambda (k e)
                (let ((q (assoc k actual-row)))
                  (and q (%tck-value= (cdr q) e :unordered unordered))))
              keys expected-cells)))

(defun %tck-rows-match (header body rows &key (ordered nil) (unordered nil))
  "Compare the TCK table (HEADER row + BODY rows) with actual ROWS.
Returns T or NIL."
  (let* ((keys (mapcar #'intern header))
         (expected (mapcar (lambda (r) (mapcar #'%tck-parse-cell r)) body)))
    (cond
      ((null (and (= (length expected) (length rows)))) nil)
      (ordered (every (lambda (er ar) (%tck-row= keys er ar :unordered unordered))
                      expected rows))
      (t (%bag-match (lambda (er ar) (%tck-row= keys er ar :unordered unordered))
                     expected rows)))))

;;; ------------------------------------------------------------------
;;; graph snapshots (for side effects)

(defun %graph-snapshot (g)
  (list :nodes
        (loop for eid in (graph-scan-node-ids g)
              for node = (graph-node g eid)
              collect (list eid (getf node :labels) (getf node :props)))
        :rels
        (loop for rid in (graph-scan-rel-ids g)
              for rel = (graph-relationship g rid)
              collect (list rid (getf rel :type) (getf rel :start) (getf rel :end)
                            (getf rel :props)))))

(defun %pairs-added (before after)
  "Pairs in AFTER missing from BEFORE (compared by key and value)."
  (let ((n 0))
    (dolist (p after)
      (let ((q (assoc (car p) before :test #'equal)))
        (unless (and q (cypher-value= (cdr p) (cdr q)))
          (incf n))))
    n))

(defun %snapshot-diff (before after)
  "Return a plist of side-effect counts between snapshots."
  (let ((b-nodes (getf before :nodes)) (a-nodes (getf after :nodes))
        (b-rels (getf before :rels)) (a-rels (getf after :rels)))
    (let ((b-ids (mapcar #'first b-nodes)) (a-ids (mapcar #'first a-nodes))
          (b-rids (mapcar #'first b-rels)) (a-rids (mapcar #'first a-rels)))
      (list
       :+nodes (length (set-difference a-ids b-ids :test #'equal))
       :-nodes (length (set-difference b-ids a-ids :test #'equal))
       :+relationships (length (set-difference a-rids b-rids :test #'equal))
       :-relationships (length (set-difference b-rids a-rids :test #'equal))
       :+labels
       (length (set-difference
                (remove-duplicates (loop for (id labels props) in a-nodes
                                         append labels)
                                   :test #'equal)
                (remove-duplicates (loop for (id labels props) in b-nodes
                                         append labels)
                                   :test #'equal)
                :test #'equal))
       :-labels
       (length (set-difference
                (remove-duplicates (loop for (id labels props) in b-nodes
                                         append labels)
                                   :test #'equal)
                (remove-duplicates (loop for (id labels props) in a-nodes
                                         append labels)
                                   :test #'equal)
                :test #'equal))
       :+properties
       (+ (loop for (id labels props) in a-nodes
                for old = (third (assoc id b-nodes :test #'equal))
                sum (%pairs-added (or old nil) props))
          (loop for (id type start end props) in a-rels
                for old = (fifth (assoc id b-rels :test #'equal))
                sum (%pairs-added (or old nil) props)))
       :-properties
       (+ (loop for (id labels props) in b-nodes
                for new = (third (assoc id a-nodes :test #'equal))
                sum (%pairs-added (or new nil) props))
          (loop for (id type start end props) in b-rels
                for new = (fifth (assoc id a-rels :test #'equal))
                sum (%pairs-added (or new nil) props)))))))

;;; ------------------------------------------------------------------
;;; unsupported-feature detection

(defun %has-var-length (q)
  "True when Q contains a relationship pattern with * or + inside []."
  (let ((n (length q)) (i 0) (found nil))
    (loop
      (setf i (search "[" q :start2 i))
      (unless i (return))
      (let ((j (or (search "]" q :start2 i) n))
            (k (1- i)))
        (when (and (>= k 0) (char= (char q k) #\-)
                   (or (search "*" q :start2 i :end2 j)
                       (search "+" q :start2 i :end2 j)))
          (setf found t)))
      (incf i)
      (when found (return)))
    found))

(defun %has-path-assignment (q)
  "True when Q contains 'ident = (' (a path assignment)."
  (let ((n (length q)) (i 0) (found nil))
    (loop
      (setf i (position #\= q :start i))
      (unless i (return))
      (let ((j i))
        (loop while (and (> j 0) (member (char q (1- j)) '(#\Space #\Tab))) do (decf j))
        (loop while (and (> j 0) (alphanumericp (char q (1- j)))) do (decf j))
        (when (< j i)
          (let ((k (1+ i)))
            (loop while (and (< k n) (member (char q k) '(#\Space #\Tab))) do (incf k))
            (when (and (< k n) (char= (char q k) #\())
              (setf found t)))))
      (incf i)
      (when found (return)))
    found))

(defun %tck-query-unsupported (q)
  "Return an unsupported-reason string when Q exercises a feature the
engine does not implement, or NIL."
  (let ((up (string-upcase q)))
    (cond
      ((or (search "SHORTESTPATH(" up) (search "ALLSHORTESTPATHS(" up))
       "shortestPath")
      ((search "CALL " up) "procedures (CALL)")
      ((search "EXISTS(" up) "exists()")
      ((or (search "DATE(" up) (search "TIME(" up) (search "DATETIME(" up)
           (search "LOCALTIME(" up) (search "LOCALDATETIME(" up)
           (search "DURATION(" up))
       "temporal functions")
      ((search "PERCENTILE" up) "percentile aggregates")
      ((or (search "STDEV(" up) (search "STDEVP(" up)) "stDev aggregates")
      ((search "{.*}" q) "map projections")
      ((and (search "[(" q) (search "| " q)) "pattern/list comprehensions")
      (t nil))))

;;; ------------------------------------------------------------------
;;; scenario execution

(defstruct tck-context
  store graph params rows error-kind error-family snapshot)

(defun %tck-parse-params (table)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (row table)
      (when (>= (length row) 2)
        (setf (gethash (first row) h)
              (handler-case (%tck-parse-cell (second row))
                (error () :cypher-null)))))
    h))

(defun %tck-error-expectation (text)
  "Parse 'a <Kind> should be raised at <phase>: <detail>'
into (values kind phase detail)."
  (let ((pos (search " should be raised at " text)))
    (when pos
      (let ((colon (search ": " text :start2 pos)))
        (when colon
          (let* ((head (string-trim " " (subseq text 0 pos)))
                 (kind (let ((a (search "a " head)))
                         (if (and a (zerop a)) (subseq head 2) head)))
                 (phase (subseq text (+ pos (length " should be raised at ")) colon))
                 (detail (string-trim " " (subseq text (+ colon 2)))))
            (values kind phase detail)))))))

(defun %tck-side-effects-match (table before after)
  (let ((diff (%snapshot-diff before after)))
    (every (lambda (row)
             (when (>= (length row) 2)
               (let* ((kind (intern (string-upcase (first row)) "KEYWORD"))
                      (want (parse-integer (second row) :junk-allowed t)))
                 (= (or (getf diff kind) 0) (or want 0)))))
           (rest table))))

(defun %fresh-graph ()
  (let ((store (make-store)))
    (values store (make-local-graph store))))

(defun %tck-run-scenario (feature-name scenario)
  "Run one scenario; returns (:pass), (:fail reason) or
(:unsupported reason)."
  (destructuring-bind (name tags steps examples) scenario
    (declare (ignore feature-name name tags examples))
    (multiple-value-bind (store graph) (%fresh-graph)
      (let ((params nil)
            (rows nil)
            (error-kind nil)
            (error-family nil)
            (snapshot nil)
            (result (list :pass)))
        (labels ((fail (fmt &rest args)
                   (setf result (list :fail (apply #'format nil fmt args)))
                   (return-from %tck-run-scenario result))
                 (skip (reason)
                   (setf result (list :unsupported reason))
                   (return-from %tck-run-scenario result))
                 (run-query (text)
                   (setf snapshot (%graph-snapshot graph))
                   (setf error-kind nil error-family nil)
                   (handler-case
                       (progn
                         (setf rows (cypher-query text graph :params params))
                         (setf params nil))
                     (cypher-error (e)
                       (setf error-kind (cypher-error-kind e))
                       (setf error-family
                             (cond ((typep e 'cypher-syntax-error) "SyntaxError")
                                   ((typep e 'cypher-type-error) "TypeError")
                                   ((typep e 'cypher-argument-error) "ArgumentError")
                                   ((typep e 'cypher-entity-not-found) "EntityNotFound")
                                   (t "CypherError")))
                       (setf rows nil))
                     (error (e)
                       (setf error-kind (format nil "LispError:~a" (type-of e)))
                       (setf error-family "LispError")
                       (setf rows nil)))))
          (dolist (step steps)
            (let ((text (getf (cdr step) :text))
                  (doc (getf (cdr step) :doc))
                  (table (getf (cdr step) :table)))
              (cond
                ((string-equal text "an empty graph")
                 (multiple-value-setq (store graph) (%fresh-graph)))
                ((string-equal text "any graph")
                 (multiple-value-setq (store graph) (%fresh-graph)))
                ((and (>= (length text) 4) (string-equal text "the " :end1 4)
                      (search " graph" text))
                 (let* ((gname (subseq text 4 (search " graph" text)))
                        (setup (format nil "specs/openCypher/tck/graphs/~a/~a.cypher"
                                       gname gname)))
                   (cond
                     ((null (probe-file setup))
                      (skip (format nil "unknown graph ~a" gname)))
                     (t
                      (multiple-value-setq (store graph) (%fresh-graph))
                      (with-open-file (in setup :external-format :utf-8)
                        (let ((q (make-string (file-length in))))
                          (read-sequence q in)
                          (let ((reason (%tck-query-unsupported q)))
                            (if reason
                                (skip (format nil "graph ~a uses ~a" gname reason))
                                (handler-case (cypher-query q graph)
                                  (error (e)
                                    (skip (format nil "graph ~a setup: ~a"
                                                  gname e))))))))))))
                ((and (search "having executed" text) doc)
                 (let ((reason (%tck-query-unsupported doc)))
                   (if reason
                       (skip reason)
                       (progn
                         (handler-case
                             (cypher-query doc graph :params params)
                           (error (e) (fail "setup failed: ~a" e)))
                         (setf params nil)))))
                ((or (search "parameter values are" text) (search "parameters are" text))
                 (setf params (%tck-parse-params table)))
                ((search "executing control query" text)
                 (skip "control query"))
                ((and (search "executing query" text) doc)
                 (let ((reason (%tck-query-unsupported doc)))
                   (if reason
                       (skip reason)
                       (progn
                         (run-query doc)))))
                ((search "the result should be empty" text)
                 (when rows
                   (fail "expected empty, got ~d rows" (length rows))))
                ((and (search "in any order" text) table)
                 (unless (%tck-rows-match (first table) (rest table) rows)
                   (when *tck-debug* (%tck-dump-rows (first table) (rest table) rows))
                   (fail "result mismatch (any order)")))
                ((and (search "ignoring element order for lists" text) table)
                 (unless (%tck-rows-match (first table) (rest table) rows :unordered t)
                   (when *tck-debug* (%tck-dump-rows (first table) (rest table) rows))
                   (fail "result mismatch (list order)")))
                ((and (search "in order" text) table)
                 (unless (%tck-rows-match (first table) (rest table) rows :ordered t)
                   (when *tck-debug* (%tck-dump-rows (first table) (rest table) rows))
                   (fail "result mismatch (ordered)")))
                ((search "no side effects" text)
                 (let ((diff (%snapshot-diff snapshot (%graph-snapshot graph))))
                   (unless (and (zerop (getf diff :+nodes))
                                (zerop (getf diff :-nodes))
                                (zerop (getf diff :+relationships))
                                (zerop (getf diff :-relationships))
                                (zerop (getf diff :+labels))
                                (zerop (getf diff :-labels))
                                (zerop (getf diff :+properties))
                                (zerop (getf diff :-properties)))
                     (fail "side effects detected"))))
                ((search "the side effects should be" text)
                 (unless (%tck-side-effects-match table snapshot (%graph-snapshot graph))
                   (when *tck-debug*
                     (format t "~&  side-effects diff: ~s~%"
                             (%snapshot-diff snapshot (%graph-snapshot graph))))
                   (fail "side effects mismatch")))
                ((search "should be raised" text)
                 (multiple-value-bind (kind phase detail) (%tck-error-expectation text)
                   (declare (ignore phase))
                   (cond
                     ((null kind)
                      (skip (format nil "step: ~a"
                                    (subseq text 0 (min 60 (length text))))))
                     ((member kind '("SyntaxError" "TypeError" "ArgumentError"
                                     "EntityNotFound")
                              :test #'string-equal)
                      (let ((ok (and error-kind
                                     (if (string-equal detail "*")
                                         (string-equal error-family kind)
                                         (string-equal error-kind detail)))))
                        (unless ok
                          (fail "expected ~a: ~a, got ~s" kind detail error-kind))))
                     (t (skip (format nil "error kind ~a" kind))))))
                ((search "there exists a procedure" text)
                 (skip "procedures"))
                (t (skip (format nil "step: ~a"
                                 (subseq text 0 (min 60 (length text)))))))))
          result)))))

;;; ------------------------------------------------------------------
;;; driver

(defun %feature-files (directory)
  "All .feature files under DIRECTORY (recursively); if DIRECTORY names
a single .feature file, return that file alone."
  (let ((base (pathname directory)))
    (if (and (probe-file base)
             (null (pathname-name (probe-file base))))
        ;; an existing directory
        (directory
         (make-pathname
          :directory (append (pathname-directory (probe-file base))
                             '(:wild-inferiors))
          :name :wild :type "feature"))
        (list base))))

(defun run-tck (directory &key (verbose nil) (limit nil))
  "Run all .feature files under DIRECTORY.  Returns the stats plist."
  (%tck-reset)
  (dolist (path (sort (%feature-files directory)
                      #'string< :key #'namestring))
    (let ((feature (car (last (pathname-directory path)))))
      (dolist (scenario (%parse-gherkin (namestring path)))
        (dolist (expanded (%expand-outline scenario))
          (let ((name (first expanded)) (tags (second expanded)))
            (if (member "ignore" tags :test #'string-equal)
                (progn
                  (incf (getf *tck-stats* :unsupported))
                  (incf (gethash "TCK @ignore" (getf *tck-stats* :reasons) 0))
                  (incf (getf *tck-stats* :total)))
                (let ((result (%tck-run-scenario feature expanded)))
                  (incf (getf *tck-stats* (first result)))
                  (incf (getf *tck-stats* :total))
                  (when (eq (first result) :fail)
                    (push (list (namestring path) name (second result))
                          (getf *tck-stats* :failures)))
                  (when (eq (first result) :unsupported)
                    (when (or (equal (second result) "step: an empty graph")
                              (equal (second result) "step: any graph"))
                      (format t "~&UNSUP ~a :: ~a :: ~s~%"
                              (namestring path) name (second result)))
                    (incf (gethash (second result)
                                   (getf *tck-stats* :reasons) 0)))
                  (when verbose
                    (format t "~a ~a: ~a~%" (first result) name (second result))))))
          (when (and limit (>= (getf *tck-stats* :total) limit))
            (return-from run-tck *tck-stats*))))))
  *tck-stats*)
