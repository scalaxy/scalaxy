;;;; executor.lisp --- pull-based Cypher query evaluator
;;;;
;;;; Rows are alists (var . value); tables are bags (duplicates kept).
;;;; Operators compile to pull cursors (closures returning the next row
;;;; or :EOF).  Evaluation is pure modulo the graph-view access (E2):
;;;; the same code runs on a local or gateway graph.
;;;;
;;;; Aggregation restriction (v1): aggregate items must be a single
;;;; count/sum/avg/min/max/collect call (or count(*)); arithmetic over
;;;; aggregates is rejected by the semantic checker.

(in-package #:scalaxy)

(defstruct (cursor (:constructor %make-cursor (fn)))
  fn)

(defun cursor-next (c)
  (funcall (cursor-fn c)))

(defun cursor-drain (c)
  (loop for row = (cursor-next c) until (eq row :eof) collect row))

(defun %list-cursor (rows)
  (let ((i 0))
    (%make-cursor (lambda ()
                    (if (< i (length rows))
                        (prog1 (nth i rows) (incf i))
                        :eof)))))

(defun %concat-cursor (a b)
  (let ((state :a))
    (%make-cursor (lambda ()
                    (loop
                      (case state
                        (:a (let ((row (cursor-next a)))
                              (if (eq row :eof) (setf state :b) (return row))))
                        (:b (return (cursor-next b)))))))))

(defun %map-cursor (inner fn)
  "Apply FN to every row of INNER; FN returns a list of rows (possibly
empty, possibly one) for each input row."
  (let ((pending nil))
    (%make-cursor (lambda ()
                    (loop
                      (when pending (return (pop pending)))
                      (let ((row (cursor-next inner)))
                        (if (eq row :eof)
                            (return :eof)
                            (setf pending (funcall fn row)))))))))

(defun %filter-cursor (inner pred)
  (%make-cursor (lambda ()
                  (loop for row = (cursor-next inner)
                        until (eq row :eof)
                        when (%tv-true (funcall pred row))
                          return row
                        finally (return :eof)))))

(defun row-get (row var)
  (cdr (assoc var row)))

(defun row-bind (row var value)
  (acons var value row))

(defun row-keys (row)
  (mapcar #'car row))

(defun %row-projection (row vars)
  "Keep only VARS (in order) of ROW."
  (loop for v in vars
        collect (assoc v row)))

(defun %row-equal (a b)
  (and (= (length a) (length b))
       (every (lambda (p)
                (let ((q (assoc (car p) b)))
                  (and q (%tv-true (cypher-= (cdr p) (cdr q))))))
              a)))

(defun %row-value-key (row)
  (mapcar (lambda (p) (cons (symbol-name (car p)) (cdr p))) row))

;;; ------------------------------------------------------------------
;;; node / rel record helpers (over the graph-view)

(defun %labels-of (node) (getf node :labels))
(defun %entity-props (entity) (getf entity :props))

(defun %prop-value (entity k)
  (let ((p (assoc k (getf entity :props) :test #'equal)))
    (if p (cdr p) :cypher-null)))

(defun %entity-matches (entity labels prop-exprs row graph params)
  "Check that ENTITY carries all LABELS and satisfies PROP-EXPRS
(alist of key -> expr) evaluated against ROW."
  (and (every (lambda (l) (member l (%labels-of entity) :test #'equal))
              labels)
       (every (lambda (p)
                (%tv-true (cypher-= (%prop-value entity (car p))
                                    (eval-expr (cdr p) row graph params))))
              prop-exprs)))

(defun %bind-entity-var (row var entity)
  (cond
    ((null var) row)
    ((null (assoc var row)) (row-bind row var entity))
    (t (let ((old (row-get row var)))
         (if (%tv-true (cypher-= old entity))
             row
             :fail)))))

;;; ------------------------------------------------------------------
;;; pattern matching (executor: index joins over the graph-view)

(defun %node-candidates (g node-el row graph params)
  "Candidate node values for NODE-EL in the context of ROW.
Returns (values list already-bound?)"
  (let ((var (getf (cdr node-el) :var)))
    (if (and var (assoc var row))
        (let ((v (row-get row var)))
          ;; a null anchor (e.g. WITH null AS a MATCH (a)) matches nothing
          (if (%tv-null v) (values nil t) (values (list v) t)))
        (let* ((labels (getf (cdr node-el) :labels))
               (props (getf (cdr node-el) :props))
               (label (first labels)))
          (values
           (loop for eid in (graph-scan-node-ids g :label label)
                 for node = (graph-node g eid)
                 when (%entity-matches node (rest labels) props row graph params)
                   collect node)
           nil)))))

(defun %node-start-cursor (g inner node-el graph params)
  (%map-cursor
   inner
   (lambda (row)
     (multiple-value-bind (cands already?) (%node-candidates g node-el row graph params)
       (let ((var (getf (cdr node-el) :var)))
         (cond
           ((null cands) nil)
           (already? (list row))
           (t (loop for n in cands collect (row-bind row var n)))))))))

(defun %expand-cursor (g inner rel-el node-el src-var graph params)
  (let ((dir (getf (cdr rel-el) :dir))
        (rtype (getf (cdr rel-el) :type))
        (rprops (getf (cdr rel-el) :props))
        (rvar (getf (cdr rel-el) :var))
        (nvar (getf (cdr node-el) :var))
        (nlabels (getf (cdr node-el) :labels))
        (nprops (getf (cdr node-el) :props)))
    (%map-cursor
     inner
     (lambda (row)
       (let ((src (row-get row src-var)))
          (when (%tv-null src) (return-from %expand-cursor nil))
         (labels
             ((try (rel node)
                (when (and rel node
                           (%entity-matches rel nil rprops row graph params)
                           (%entity-matches node nlabels nprops row graph params))
                  (let* ((rel2 (if (eq dir :in)
                                   (list* :start-node node :end-node src rel)
                                   (list* :start-node src :end-node node rel)))
                         (row2 (if rvar (row-bind row rvar rel2) row)))
                    (let ((row3 (%bind-entity-var row2 nvar node)))
                      (if (eq row3 :fail) nil (list row3)))))))
           (let ((bound (and rvar (assoc rvar row))))
             (if (null bound)
                 ;; unbound relationship: expand over all incident rels
                 (loop for (rid . neighbor)
                         in (graph-expand g (getf src :id) :dir dir :type rtype)
                       for rel = (graph-relationship g rid)
                       for node = (graph-node g neighbor)
                       nconc (try rel node))
                 ;; bound relationship: it must be incident to SRC in
                 ;; direction DIR; the other endpoint is the next node.
                 ;; The rel already carries :start-node/:end-node.
                 (let* ((rel (cdr bound))
                        (start (getf (getf rel :start-node) :id))
                        (end (getf (getf rel :end-node) :id))
                        (sid (getf src :id))
                        (neighbor-id
                          (cond ((equal sid start)
                                 (unless (eq dir :in) end))
                                ((equal sid end)
                                 (unless (eq dir :out) start))
                                (t nil))))
                   (when neighbor-id
                     (let ((node (graph-node g neighbor-id)))
                       (when (and node
                                  (%entity-matches rel nil rprops row graph params)
                                  (%entity-matches node nlabels nprops row graph params))
                         (let ((row2 (if rvar (row-bind row rvar rel) row)))
                           (let ((row3 (%bind-entity-var row2 nvar node)))
                             (if (eq row3 :fail) nil (list row3))))))))))))))))

(defun %rel-step-pairs (g rel-el src row graph params)
  "One-step candidates from node SRC along REL-EL: ((rel2 . node) ...)."
  (let ((dir (getf (cdr rel-el) :dir))
        (rtype (getf (cdr rel-el) :type))
        (rprops (getf (cdr rel-el) :props)))
    (loop for (rid . neighbor) in (graph-expand g (getf src :id) :dir dir :type rtype)
          for rel = (graph-relationship g rid)
          for node = (graph-node g neighbor)
          when (and rel node
                    (%entity-matches rel nil rprops row graph params))
            collect (cons (if (eq dir :in)
                              ;; incoming: the neighbor is the rel's start
                              (list* :start-node node :end-node src rel)
                              (list* :start-node src :end-node node rel))
                          node))))

(defun %path-chain-cursor (g inner chain graph params)
  "Chain cursor supporting a path variable and variable-length
relationships (plan Tier 3): walks the chain breadth-first, binding
the path variable to (:path (node rel node ...)).  A path never
repeats a relationship (openCypher paths are trails)."
  (let* ((pv (and (eq (car chain) :path-var) (second chain)))
         (elements (if (eq (car chain) :path-var) (cddr chain) chain)))
    (%map-cursor
     inner
     (lambda (row)
       (let ((results nil))
         (labels ((walk (idx r path visited)
                    (if (>= idx (length elements))
                        (let ((r2 (if pv
                                      (row-bind r pv (list :path (nreverse path)))
                                      r)))
                          (push r2 results))
                        (let ((el (nth idx elements)))
                          (ecase (car el)
                            (:node
                             (multiple-value-bind (cands already?)
                                 (%node-candidates g el r graph params)
                               (let ((var (getf (cdr el) :var)))
                                 (dolist (n cands)
                                   (let ((r2 (if already?
                                                 r
                                                 (%bind-entity-var r var n))))
                                     (unless (eq r2 :fail)
                                       (walk (1+ idx) r2 (cons n path) visited)))))))
                            (:rel
                             (let* ((next-el (nth (1+ idx) elements))
                                    (min-hops (if (getf (cdr el) :var-length)
                                                  (or (getf (cdr el) :min) 1)
                                                  1))
                                    (max-hops (if (getf (cdr el) :var-length)
                                                  (or (getf (cdr el) :max) 999999)
                                                  1))
                                    (rvar (getf (cdr el) :var)))
                               (let ((queue (list (list r 0 path visited nil))))
                                 (loop while queue
                                       do (destructuring-bind (cur hops p v rels) (pop queue)
                                            (let ((src (first p)))
                                              (when (>= hops min-hops)
                                                ;; the reached node is the end node:
                                                ;; check the element's constraints and bind
                                                (let ((end-node (first p)))
                                                  (when (%entity-matches
                                                         end-node
                                                         (getf (cdr next-el) :labels)
                                                         (getf (cdr next-el) :props)
                                                         cur graph params)
                                                    (let* ((nvar (getf (cdr next-el) :var))
                                                           (r2 (if (null nvar)
                                                                   cur
                                                                   (if (assoc nvar cur)
                                                                       (let ((old (row-get cur nvar)))
                                                                         (if (%tv-true (cypher-= old end-node))
                                                                             cur
                                                                             :fail))
                                                                       (row-bind cur nvar end-node)))))
                                                      (unless (eq r2 :fail)
                                                        (let ((r3 (if rvar
                                                                      (row-bind r2 rvar
                                                                                (cypher-list
                                                                                 (nreverse rels)))
                                                                      r2)))
                                                          (walk (+ idx 2) r3 p v)))))))
                                              (when (< hops max-hops)
                                                (dolist (pair (%rel-step-pairs g el src cur graph params))
                                                  (unless (member (getf (car pair) :id) v :test #'equal)
                                                    (push (list cur (1+ hops)
                                                                (cons (cdr pair)
                                                                      (cons (car pair) p))
                                                                (cons (getf (car pair) :id) v)
                                                                (cons (car pair) rels))
                                                          queue)))))))))))))))
           (walk 0 row nil nil))
         (nreverse results))))))

(defun %chain-cursor (g inner chain graph params)
  (if (or (eq (car chain) :path-var)
          (loop for el in chain thereis (and (eq (car el) :rel)
                                             (getf (cdr el) :var-length))))
      (%path-chain-cursor g inner chain graph params)
      ;; give anonymous nodes stable internal keys so multi-hop chains
      ;; like (a)-[:R]->()-[:R]->(c) can expand through the middle node
      (let* ((anon 0)
             (keyed (mapcar (lambda (el)
                              (if (eq (car el) :node)
                                  (let ((v (getf (cdr el) :var)))
                                    (if v
                                        el
                                        (list* :node :var
                                               (intern (format nil "~a" (incf anon))
                                                       "SCALAXY")
                                               (cdddr el))))
                                  el))
                            chain)))
        (let ((cur (%node-start-cursor g inner (first keyed) graph params)))
          (loop for idx from 1 below (length keyed) by 2
                for rel = (nth idx keyed)
                for node = (nth (1+ idx) keyed)
                for src = (nth (1- idx) keyed)
                do (setf cur (%expand-cursor g cur rel node (getf (cdr src) :var)
                                             graph params)))
          cur))))

(defun %pattern-cursor (g inner pattern graph params)
  (let ((cur inner))
    (dolist (chain pattern)
      (setf cur (%chain-cursor g cur chain graph params)))
    cur))

(defun %pattern-vars (pattern)
  (let ((vars nil))
    (dolist (chain pattern)
      (when (eq (car chain) :path-var)
        (pushnew (second chain) vars))
      (dolist (el (if (eq (car chain) :path-var) (cddr chain) chain))
        (let ((v (getf (cdr el) :var)))
          (when (and v (not (member v vars)))
            (push v vars)))))
    (nreverse vars)))

(defun %pattern-exists (row pattern graph params)
  "True when PATTERN matches at least once starting from ROW."
  (not (null (cursor-drain (%pattern-cursor graph (%list-cursor (list row)) pattern graph params)))))

;;; ------------------------------------------------------------------
;;; clause operators

(defun %project-row (row items graph params)
  "Projection without aggregation: returns one output row.  Unaliased
items get their column name from the printed expression."
  (loop for item in items
        for expr = (getf (cdr item) :expr)
        for as = (getf (cdr item) :as)
        for v = (eval-expr expr row graph params)
        collect (cons (or as (ast-var (ast-print (list :expr expr)))) v)))

(defun %group-row (row items graph params)
  "For aggregation: returns (values group-key agg-items-evaluated).
Each aggregate item evaluates to the per-row value to accumulate
(an alist of (fn . value) per aggregate item)."
  (let ((key nil) (aggs nil))
    (dolist (item items)
      (let ((expr (getf (cdr item) :expr)))
        (if (expr-has-aggregate expr)
            (push (list (getf (cdr item) :as) expr) aggs)
            (push (cons (getf (cdr item) :as)
                        (eval-expr expr row graph params))
                  key))))
    (values (nreverse key) (nreverse aggs))))

(defun %agg-kind (expr)
  "The aggregate keyword of EXPR (count/sum/avg/min/max/collect)."
  (cond
    ((eq (car expr) :count-*) :count)
    ((eq (car expr) :call)
     (intern (string-upcase (getf (cdr expr) :fn)) "KEYWORD"))
    (t (error "aggregate item must be a single aggregate call"))))

;;; ------------------------------------------------------------------
;;; aggregation: symbolic evaluation of expressions containing
;;; aggregate calls (aggregates inside scalar calls and arithmetic)

(defvar *agg-sites* nil
  "EQ hash: aggregate call AST node -> site marker (a gensym).")

(defvar *agg-site-info* nil
  "EQ hash: site marker -> (kind . distinct?).")

(defvar *agg-states* nil
  "EQ hash: site marker -> accumulator state (for one group).")

(defvar *agg-contribs* nil
  "Per row: alist of (site . (kind distinct? . argvals)).")

(defun %agg-site (expr)
  "Register aggregate call EXPR; returns its site marker."
  (or (gethash expr *agg-sites*)
      (let ((site (gensym "AGG")))
        (setf (gethash expr *agg-sites*) site)
        (setf (gethash site *agg-site-info*)
              (if (eq (car expr) :count-*)
                  (cons :count nil)
                  (cons (intern (string-upcase (getf (cdr expr) :fn)) "KEYWORD")
                        (getf (cdr expr) :distinct))))
        site)))

(defun %agg-kind-of (site)
  (let ((info (gethash site *agg-site-info*)))
    (values (car info) (cdr info))))

(defun %agg-marker-p (v)
  (and (hash-table-p *agg-site-info*) v (gethash v *agg-site-info*)))

(defun %agg-symbolic-p (v)
  "True when V contains an aggregate site marker anywhere."
  (cond
    ((%agg-marker-p v) t)
    ((atom v) nil)
    (t (loop for x in (cdr v) thereis (and (consp x) (%agg-symbolic-p x))))))

(defun %agg-tree (expr row graph params)
  "Record the row's aggregate contributions of EXPR in *AGG-CONTRIBS*.
Aggregate call arguments are plain values (nested aggregates are
rejected by the semantic checker)."
  (cond
    ((atom expr))
    ((gethash expr *agg-sites*)
     (let ((site (gethash expr *agg-sites*)))
       (multiple-value-bind (kind distinct?) (%agg-kind-of site)
         (let ((vals (if (eq (car expr) :count-*)
                         (list :star)
                         (mapcar (lambda (a) (eval-expr a row graph params))
                                 (getf (cdr expr) :args)))))
           (push (cons site (cons (cons kind distinct?) vals))
                 *agg-contribs*)))))
    (t (dolist (x expr)
         (when (consp x) (%agg-tree x row graph params))))))

(defun %agg-step (state kind distinct? args)
  "STATE is a plist of accumulator slots; update with one row's value."
  (let ((v (first args)))
    (case kind
      (:count
       (if distinct?
           (let ((seen (or (getf state :seen) (make-hash-table :test #'equal))))
             (if (and (not (eq v :star)) (not (cypher-null-p v))
                      (not (gethash v seen)))
                 (progn (setf (gethash v seen) t)
                        (list :seen seen
                              :n (1+ (or (getf state :n) 0))
                              :nn (1+ (or (getf state :n) 0))))
                 (list :seen seen
                       :n (or (getf state :n) 0)
                       :nn (or (getf state :n) 0))))
           (list :n (1+ (or (getf state :n) 0))
                 :nn (+ (or (getf state :nn) 0)
                        (if (or (eq v :star) (not (cypher-null-p v))) 1 0))
                 :star (or (getf state :star) (eq v :star)))))
      (:sum (list :n (1+ (or (getf state :n) 0))
                  :total (+ (or (getf state :total) 0)
                            (if (and (numberp v) (not (cypher-null-p v))) v 0))))
      (:avg (list :n (1+ (or (getf state :n) 0))
                  :total (+ (or (getf state :total) 0)
                             (if (and (numberp v) (not (cypher-null-p v))) v 0))))
      (:min (list :best (if (and v (not (cypher-null-p v))
                                 (or (null (getf state :best))
                                     (eq (cypher-compare v (getf state :best)) :lt)))
                            v
                            (getf state :best))))
      (:max (list :best (if (and v (not (cypher-null-p v))
                                 (or (null (getf state :best))
                                     (eq (cypher-compare v (getf state :best)) :gt)))
                            v
                            (getf state :best))))
      (:collect
       (if distinct?
           (let ((seen (or (getf state :seen) (make-hash-table :test #'equal))))
             (if (gethash v seen)
                 (list :seen seen :items (or (getf state :items) nil))
                 (progn (setf (gethash v seen) t)
                        (list :seen seen :items (cons v (or (getf state :items) nil))))))
           (list :items (cons v (or (getf state :items) nil))))))))

(defun %agg-finish (kind state)
  (case kind
    (:count (if (getf state :star)
                (or (getf state :n) 0)
                (or (getf state :nn) 0)))
    (:sum (or (getf state :total) 0))
    (:avg (let ((n (getf state :n)))
            (if (zerop n) :cypher-null
                ;; avg always yields a float (openCypher)
                (float (/ (or (getf state :total) 0) n)))))
    (:min (or (getf state :best) :cypher-null))
    (:max (or (getf state :best) :cypher-null))
    (:collect (cypher-list (nreverse (or (getf state :items) nil))))))

(defun %agg-group-key (key-items row graph params)
  "The grouping key for ROW: an alist of (projected-name . value)."
  (mapcar (lambda (item)
            (let ((expr (getf (cdr item) :expr)))
              (cons (or (getf (cdr item) :as)
                        (ast-var (ast-print (list :expr expr))))
                    (eval-expr expr row graph params))))
          key-items))

(defun %agg-lookup (key key-items)
  "Extend the group KEY alist (projected-name . value) with entries
keyed by the printed expression name and by bare variable names."
  (let ((out nil))
    (dolist (p key) (push p out))
    (loop for item in key-items
          for p in key
          do (let ((expr (getf (cdr item) :expr)))
               (push (cons (ast-var (ast-print (list :expr expr))) (cdr p)) out)
               (when (symbolp expr)
                 (push (cons expr (cdr p)) out))))
    out))

(defun %agg-eval-with (expr lookup graph params)
  "Evaluate EXPR over a grouped row: aggregate calls resolve from
*AGG-STATES* (via *AGG-FINISH-HOOK*); variables resolve from LOOKUP."
  (let ((*agg-finish-hook*
          (lambda (e)
            (let ((site (gethash e *agg-sites*)))
              (multiple-value-bind (kind distinct?) (%agg-kind-of site)
                (declare (ignore distinct?))
                (%agg-finish kind (gethash site *agg-states*)))))))
    (declare (special *agg-finish-hook*))
    (eval-expr expr lookup graph params)))

(defun %agg-eval-empty (expr)
  "The value of an aggregate expression over empty input."
  (cond
    ((eq (car expr) :count-*) 0)
    ((and (eq (car expr) :call) (%aggregate-fn-p (getf (cdr expr) :fn)))
     (let ((kind (intern (string-upcase (getf (cdr expr) :fn)) "KEYWORD")))
       (%agg-finish kind nil)))
    ((member (car expr) '(:lit :param)) (eval-expr expr nil nil nil))
    ((eq (car expr) :var) :cypher-null)
    (t
     (handler-case
         (eval-expr (cons (car expr)
                          (mapcar (lambda (c)
                                    (if (consp c)
                                        (list :lit (%agg-eval-empty c))
                                        c))
                                  (cdr expr)))
                    nil nil nil)
       (cypher-error () :cypher-null)))))

(defun %aggregate-rows (rows items graph params &key order)
  "Aggregate ROWS per the RETURN/WITH items.  Returns the output rows
(sorted by ORDER specs evaluated per group, when given)."
  (let ((key-items nil) (agg-items nil))
    (dolist (item items)
      (if (expr-has-aggregate (getf (cdr item) :expr))
          (push item agg-items)
          (push item key-items)))
    (setf key-items (nreverse key-items) agg-items (nreverse agg-items))
    (if (null agg-items)
        (mapcar (lambda (row) (%project-row row items graph params)) rows)
        (let ((groups (make-hash-table :test #'equal))
              (order-keys nil)
              (*agg-sites* (make-hash-table))
              (*agg-site-info* (make-hash-table)))
          (declare (special *agg-sites* *agg-site-info*))
          ;; register every aggregate call site (projection items and
          ;; ORDER BY specs)
          (labels ((register (e)
                     (cond
                       ((atom e))
                       ((and (eq (car e) :call)
                             (%aggregate-fn-p (getf (cdr e) :fn)))
                        (%agg-site e))
                       ((eq (car e) :count-*) (%agg-site e))
                       (t (dolist (x e) (register x))))))
            (dolist (item agg-items)
              (register (getf (cdr item) :expr)))
            (dolist (spec order)
              (register (getf (cdr spec) :expr))))
          ;; accumulate per group
          (dolist (row rows)
            (let ((key (%agg-group-key key-items row graph params)))
              (let ((entry (gethash key groups)))
                (unless entry
                  (setf entry (list (make-hash-table) key))
                  (setf (gethash key groups) entry)
                  (push entry order-keys))
                (let ((states (first entry)))
                  (dolist (item agg-items)
                    (let ((*agg-contribs* nil))
                      (declare (special *agg-contribs*))
                      (%agg-tree (getf (cdr item) :expr) row graph params)
                      (dolist (c *agg-contribs*)
                        (let ((site (car c))
                              (info (cadr c)))
                          (setf (gethash site states)
                                (%agg-step (gethash site states)
                                           (car info) (cdr info) (cddr c)))))))))))
          (setf order-keys (nreverse order-keys))
          (cond
            ((null rows)
             ;; empty input: one row of finished empty aggregates when
             ;; there are no grouping keys; no rows otherwise
             (if key-items
                 nil
                 (list
                  (mapcar (lambda (item)
                            (let ((expr (getf (cdr item) :expr)))
                              (cons (or (getf (cdr item) :as)
                                        (ast-var (ast-print (list :expr expr))))
                                    (%agg-eval-empty expr))))
                          agg-items))))
            (t
             (let ((out
                     (loop for entry in order-keys
                           for states = (first entry)
                           for key = (second entry)
                           collect
                           (let ((*agg-states* states)
                                 (lookup (%agg-lookup key key-items)))
                             (declare (special *agg-states*))
                             (append
                              key
                              (mapcar (lambda (item)
                                        (let ((expr (getf (cdr item) :expr)))
                                          (cons (or (getf (cdr item) :as)
                                                    (ast-var (ast-print
                                                              (list :expr expr))))
                                                (%agg-eval-with expr lookup
                                                                graph params))))
                                      agg-items))))))
               (if (null order)
                   out
                   ;; order the grouped rows by the ORDER specs
                   (let ((keyed
                           (mapcar
                            (lambda (entry)
                              (let* ((states (first entry))
                                     (key (second entry))
                                     (*agg-states* states))
                                (declare (special *agg-states*))
                                ;; ORDER BY may reference grouping keys AND
                                ;; aggregate aliases; build a full lookup
                                (let* ((key-lookup (%agg-lookup key key-items))
                                       (full-lookup
                                         (append
                                          key-lookup
                                          (mapcar
                                           (lambda (item)
                                             (let ((expr (getf (cdr item) :expr)))
                                               (cons (or (getf (cdr item) :as)
                                                         (ast-var (ast-print
                                                                   (list :expr expr))))
                                                     (%agg-eval-with expr
                                                                     key-lookup
                                                                     graph params))))
                                           agg-items))))
                                  (cons (mapcar
                                         (lambda (spec)
                                           (%agg-eval-with (getf (cdr spec) :expr)
                                                           full-lookup
                                                           graph params))
                                         order)
                                        (append
                                         key
                                         (mapcar
                                          (lambda (item)
                                            (let ((expr (getf (cdr item) :expr)))
                                              (cons (or (getf (cdr item) :as)
                                                        (ast-var (ast-print
                                                                  (list :expr expr))))
                                                    (%agg-eval-with expr
                                                                    full-lookup
                                                                    graph params))))
                                          agg-items))))))
                            order-keys)))
                     (mapcar #'cdr (%sort-keyed keyed order)))))))))))

(defun %replace-nth (list idx value)
  (loop for i from 0 for cell on list
        when (= i idx) do (setf (car cell) value)
        finally (return list)))

(defun %distinct-rows (rows)
  (let ((seen nil))
    (loop for row in rows
          unless (member row seen :test #'%row-equal)
            do (push row seen)
          finally (return (nreverse seen)))))

(defun %sort-keyed (keyed specs)
  "Sort (key . value) pairs by their keys per SPECS (order specs)."
  (stable-sort
   keyed
   (lambda (a b)
     (loop for va in (car a) for vb in (car b)
           for desc in (mapcar (lambda (s) (getf (cdr s) :desc)) specs)
           for c = (cypher-compare va vb)
           do (cond ((eq c :null) (return (not (and (cypher-null-p va) (cypher-null-p vb)))))
                    ((eq c :eq))
                    (t (return (if desc (eq c :gt) (eq c :lt)))))
           finally (return nil)))))

(defun %sort-rows (rows specs graph params)
  (mapcar #'cdr
          (%sort-keyed
           (mapcar (lambda (row)
                     (cons (mapcar (lambda (spec)
                                     (eval-expr (getf (cdr spec) :expr) row graph params))
                                   specs)
                           row))
                   rows)
           specs)))

(defun %match-clause (g rows clause graph params)
  (let ((out (cursor-drain (%pattern-cursor g (%list-cursor rows)
                                            (getf (cdr clause) :pattern) graph params)))
        (where (getf (cdr clause) :where)))
    (if where
        (remove-if-not (lambda (row) (%tv-true (eval-expr where row graph params))) out)
        out)))

(defun %optional-clause (g rows clause graph params)
  (let ((vars (%pattern-vars (getf (cdr clause) :pattern)))
        (out nil))
    (dolist (row rows)
      (let ((sub (%match-clause g (list row) clause graph params)))
        (if sub
            (dolist (r sub) (push r out))
            (push (append row (mapcar (lambda (v) (cons v :cypher-null)) vars)) out))))
    (nreverse out)))

(defun %unwind-clause (rows clause graph params)
  (let ((out nil))
    (dolist (row rows)
      (let ((v (eval-expr (getf (cdr clause) :expr) row graph params)))
        (when (cypher-list-p v)
          (dolist (x (cypher-list-elements v))
            (push (row-bind row (getf (cdr clause) :var) x) out)))))
    (nreverse out)))

(defun %projection-clause (rows clause graph params)
  (let ((items (getf (cdr clause) :items))
        (order (getf (cdr clause) :order))
        (where (getf (cdr clause) :where)))
    ;; the WHERE subclause filters before the projection for
    ;; non-aggregating clauses (evaluated on the input row extended with
    ;; the projected columns), and after grouping for aggregating ones
    (when (and where
               (or (null items)
                   (not (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr)))
                              items))))
      (setf rows
            (remove-if-not
             (lambda (row)
               (%tv-true (eval-expr where
                                    (if items
                                        (append (%project-row row items graph params) row)
                                        row)
                                    graph params)))
             rows)))
    (let ((out
            (cond
              ((null items)
               ;; RETURN * / WITH *
               (if order (%sort-rows rows order graph params) rows))
              ((some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr))) items)
               ;; aggregation: ORDER BY (when present) is applied per group
               (%aggregate-rows rows items graph params :order order))
              ((null order)
               (%aggregate-rows rows items graph params))
              (t
               ;; non-aggregating projection with ORDER BY: the sort keys
               ;; are computed against the input row extended with the
               ;; projected columns, so ORDER BY may reference both scopes
               (mapcar (lambda (pr) (%project-row (cdr pr) items graph params))
                       (%sort-keyed
                        (mapcar (lambda (row)
                                  (cons (mapcar
                                         (lambda (spec)
                                           (eval-expr (getf (cdr spec) :expr)
                                                      (append (%project-row row items graph params)
                                                              row)
                                                      graph params))
                                         order)
                                        row))
                                rows)
                        order))))))
      ;; post-grouping WHERE (aggregation)
      (when (and where
                 (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr))) items))
        (setf out
              (remove-if-not (lambda (row) (%tv-true (eval-expr where row graph params)))
                             out)))
      (if (getf (cdr clause) :distinct)
          (%distinct-rows out)
          out))))

(defun %where-clause (rows clause graph params)
  (let ((expr (getf (cdr clause) :where)))
    (remove-if-not (lambda (row) (%tv-true (eval-expr expr row graph params)))
                   rows)))

;;; ------------------------------------------------------------------
;;; query evaluation

(defun %eval-clauses (g rows clauses graph params)
  (let ((out rows))
    (dolist (clause clauses)
      (ecase (car clause)
        (:match (setf out (%match-clause g out clause graph params)))
        (:optional-match (setf out (%optional-clause g out clause graph params)))
        (:where (setf out (%where-clause out clause graph params)))
        (:with (setf out (%projection-clause out clause graph params)))
        (:return (setf out (%projection-clause out clause graph params)))
        (:unwind (setf out (%unwind-clause out clause graph params)))
        (:order (setf out (%sort-rows out (getf (cdr clause) :items) graph params)))
        (:skip (let ((n (eval-expr (getf (cdr clause) :expr) nil graph params)))
                 (cond
                   ((not (integerp n))
                    (cypher-signal "InvalidArgumentType" :detail "SKIP must be an integer"))
                   ((minusp n)
                    (cypher-signal "NegativeIntegerArgument" :detail "SKIP must not be negative"))
                   (t (setf out (nthcdr n out))))))
        (:limit (let ((n (eval-expr (getf (cdr clause) :expr) nil graph params)))
                  (cond
                    ((not (integerp n))
                     (cypher-signal "InvalidArgumentType" :detail "LIMIT must be an integer"))
                    ((minusp n)
                     (cypher-signal "NegativeIntegerArgument" :detail "LIMIT must not be negative"))
                    (t (setf out (subseq out 0 (min n (length out))))))))
        (:union (%perr-union))
        (:create (setf out (%create-clause g out clause graph params)))
        (:merge (setf out (%merge-clause g out clause graph params)))
        (:set (setf out (%set-clause g out clause graph params)))
        (:remove (setf out (%remove-clause g out clause graph params)))
        (:delete (setf out (%delete-clause g out clause graph params)))))
    out))

(defun %perr-union ()
  (cypher-signal "UnexpectedSyntax" :detail "UNION handled at query level"))

(defun cypher-query (query graph &key (params nil) (matcher :executor))
  "Evaluate QUERY (string or parsed AST) against GRAPH (a graph-view).
Returns the result rows as a list of alists.  A query whose last
clause is an updating clause returns no rows (openCypher: updating
queries produce a result only through RETURN)."
  (let ((ast (if (stringp query) (cypher-parse query) query)))
    (cypher-check-query ast)
    (if (eq (car ast) :union)
        (let ((results nil))
          (dolist (q (getf (cdr ast) :queries))
            (let ((rows (cypher-query q graph :params params :matcher matcher)))
              (setf results (append results rows))))
          (if (getf (cdr ast) :all)
              results
              (%distinct-rows results)))
        (let* ((clauses (rest ast))
               (initial (list nil))
               (*exists-matcher*
                 (lambda (row exists-form)
                   (if (eq (car exists-form) :exists-sub)
                       (let ((where (getf (cdr exists-form) :where))
                             (matches (cursor-drain
                                       (%pattern-cursor
                                        graph (%list-cursor (list row))
                                        (list (getf (cdr exists-form) :chain))
                                        graph params))))
                         (if (some (lambda (r)
                                     (or (null where)
                                         (%tv-true (eval-expr where r graph params))))
                                   matches)
                             t
                             nil))
                       (%pattern-exists row (list (getf (cdr exists-form) :chain)) graph params))))
               (*pcomp-matcher*
                 (lambda (row pcomp)
                   (let ((matches (cursor-drain
                                   (%chain-cursor graph (%list-cursor (list row))
                                                  (getf (cdr pcomp) :chain)
                                                  graph params)))
                         (out nil))
                     (dolist (r matches)
                       (when (or (null (getf (cdr pcomp) :where))
                                 (%tv-true (eval-expr (getf (cdr pcomp) :where)
                                                      r graph params)))
                         (push (eval-expr (getf (cdr pcomp) :out) r graph params) out)))
                     (cypher-list (nreverse out))))))
          (let ((rows (%eval-clauses graph initial clauses graph params)))
            ;; queries without a RETURN produce no result rows
            ;; (updating queries report results only through RETURN)
            (if (member :return clauses :key #'car)
                rows
                nil))))))
