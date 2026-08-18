;;;; graphql.lisp --- minimal GraphQL query engine for the graph layer
;;;;
;;;; Implements a pragmatic subset of the GraphQL query language, executed
;;;; directly against the Scalaxy property graph:
;;;;
;;;;   query ($label: String) {
;;;;     nodes(label: $label, limit: 50) {
;;;;       id  labels  properties
;;;;       relationships(type: "KNOWS", direction: OUT) {
;;;;         id  type  properties
;;;;         to { id  labels  properties }
;;;;       }
;;;;     }
;;;;   }
;;;;
;;;; Supported: query operations (named/anonymous), variable definitions,
;;;; selection sets, aliases, arguments (int/float/string/bool/null/enum/
;;;; list/object/variable), named and inline fragments (directives are
;;;; parsed and ignored).  The response is a standard {"data": ...}
;;;; document plus an "extensions.graph" block carrying every node and
;;;; relationship the query materialized (deduplicated by id) so the web
;;;; console can draw the full result as a 2D graph.

(in-package #:scalaxy)

;;; ------------------------------------------------------------------
;;; lexer

(defstruct (gql-token (:constructor gql-token (kind value pos)))
  kind value pos)

(defun gql-lex (source)
  "Tokenize a GraphQL query document.  Kinds: :name :int :float :string
:lbrace :rbrace :lparen :rparen :lbracket :rbracket :colon :bang :dollar
:at :dot3 :eq :pipe (nil = end of input)."
  (let ((tokens nil) (i 0) (n (length source)))
    (labels ((peek () (and (< i n) (char source i)))
             (peek2 () (and (< (1+ i) n) (char source (1+ i))))
             (peek3 () (and (< (+ i 2) n) (char source (+ i 2))))
             (advance () (prog1 (char source i) (incf i)))
             (add (k v) (push (gql-token k v i) tokens)))
      (loop
        (when (>= i n) (return))
        (let ((c (peek)))
          (cond
            ((or (char= c #\Space) (char= c #\Tab)
                 (char= c #\Newline) (char= c #\Return))
             (advance))
            ((char= c #\,) (advance))
            ((char= c #\#)
             (loop while (and (< i n) (char/= (peek) #\Newline)) do (advance)))
            ((or (alpha-char-p c) (char= c #\_))
             (let ((start i))
               (loop while (and (< i n)
                                (or (alphanumericp (peek)) (char= (peek) #\_)))
                     do (advance))
               (add :name (subseq source start i))))
            ((or (digit-char-p c) (and (char= c #\-) (digit-char-p (peek2))))
             (let ((start i))
               (advance)
               (loop while (and (< i n) (digit-char-p (peek))) do (advance))
               (let ((floatp nil))
                 (when (and (< i n) (char= (peek) #\.))
                   (setf floatp t)
                   (advance)
                   (loop while (and (< i n) (digit-char-p (peek))) do (advance)))
                 (when (and (< i n) (member (peek) '(#\e #\E)))
                   (setf floatp t)
                   (advance)
                   (when (and (< i n) (member (peek) '(#\+ #\-))) (advance))
                   (loop while (and (< i n) (digit-char-p (peek))) do (advance)))
                 (add (if floatp :float :int) (subseq source start i)))))
            ((char= c #\")
             (advance)
             (with-output-to-string (out)
               (loop
                 (when (>= i n) (error "graphql: unterminated string"))
                 (let ((ch (advance)))
                   (cond ((char= ch #\") (return))
                         ((char= ch #\\)
                          (let ((e (advance)))
                            (case e
                              (#\" (write-char #\" out))
                              (#\\ (write-char #\\ out))
                              (#\/ (write-char #\/ out))
                              (#\b (write-char #\Backspace out))
                              (#\f (write-char #\Page out))
                              (#\n (write-char #\Newline out))
                              (#\r (write-char #\Return out))
                              (#\t (write-char #\Tab out))
                              (#\u (let ((hex (subseq source (1+ i) (+ i 5))))
                                     (incf i 4)
                                     (write-char (code-char (parse-integer hex :radix 16)) out)))
                              (t (error "graphql: bad escape \\~a" e)))))
                         (t (write-char ch out)))))
               (add :string (get-output-stream-string out))))
            ((char= c #\.)
             (unless (and (char= (peek2) #\.) (char= (peek3) #\.))
               (error "graphql: unexpected '.'"))
             (advance) (advance) (advance)
             (add :dot3 "..."))
            (t
             (let ((k (case c
                        (#\{ :lbrace) (#\} :rbrace)
                        (#\( :lparen) (#\) :rparen)
                        (#\[ :lbracket) (#\] :rbracket)
                        (#\: :colon) (#\! :bang) (#\$ :dollar)
                        (#\@ :at) (#\= :eq) (#\| :pipe)
                        (t (error "graphql: unexpected character ~s" c)))))
               (advance)
               (add k (string c))))))))
    (nreverse tokens)))

;;; ------------------------------------------------------------------
;;; parser

(defstruct (gql-parser (:constructor gql-parser (tokens &aux (pos 0))))
  tokens pos)

(defun %peek-tok (p) (nth (gql-parser-pos p) (gql-parser-tokens p)))
(defun %next-tok (p)
  (let ((tok (%peek-tok p)))
    (when tok (incf (gql-parser-pos p)))
    tok))
(defun %accept (p kind)
  (let ((tok (%peek-tok p)))
    (when (and tok (eq (gql-token-kind tok) kind))
      (incf (gql-parser-pos p))
      tok)))
(defun %expect (p kind &optional (what (string kind)))
  (let ((tok (%next-tok p)))
    (unless (and tok (eq (gql-token-kind tok) kind))
      (error "graphql: expected ~a" what))
    tok))
(defun %accept-name (p word)
  (let ((tok (%peek-tok p)))
    (when (and tok (eq (gql-token-kind tok) :name)
               (string= (gql-token-value tok) word))
      (incf (gql-parser-pos p))
      tok)))
(defun %expect-name (p word)
  (let ((tok (%next-tok p)))
    (unless (and tok (eq (gql-token-kind tok) :name)
                 (string= (gql-token-value tok) word))
      (error "graphql: expected '~a'" word))
    tok))

(defun %parse-value (p)
  (let ((tok (%next-tok p)))
    (unless tok (error "graphql: expected value"))
    (case (gql-token-kind tok)
      (:int (parse-integer (gql-token-value tok)))
      (:float (read-from-string (gql-token-value tok)))
      (:string (gql-token-value tok))
      (:dollar (list :var (gql-token-value (%expect p :name "variable name"))))
      (:name (let ((v (gql-token-value tok)))
               (cond ((string= v "true") t)
                     ((string= v "false") :gql-false)
                     ((string= v "null") :gql-null)
                     (t v))))
      (:lbracket (loop until (%accept p :rbracket)
                       collect (%parse-value p)))
      (:lbrace (let ((obj nil))
                 (loop until (%accept p :rbrace)
                       do (let ((k (gql-token-value (%expect p :name "object key"))))
                            (%expect p :colon "':'")
                            (push (cons k (%parse-value p)) obj)))
                 (list :obj (nreverse obj))))
      (t (error "graphql: unexpected value token")))))

(defun %parse-type (p)
  "Consume a GraphQL type reference (with list/non-null wrappers)."
  (when (%accept p :lbracket)
    (%parse-type p)
    (%expect p :rbracket "']'"))
  (%expect p :name "type name")
  (%accept p :bang)
  t)

(defun %parse-directives (p)
  (loop while (%accept p :at)
        do (%expect p :name "directive name")
           (when (%accept p :lparen)
             (loop until (%accept p :rparen)
                   do (%expect p :name "argument name")
                      (%expect p :colon "':'")
                      (%parse-value p)))))

(defun %parse-field (p)
  (let ((name (gql-token-value (%expect p :name "field name")))
        (alias nil) (args nil) (sel nil))
    (when (%accept p :colon)
      (setf alias name)
      (setf name (gql-token-value (%expect p :name "field name"))))
    (when (%accept p :lparen)
      (loop until (%accept p :rparen)
            do (let ((an (gql-token-value (%expect p :name "argument name"))))
                 (%expect p :colon "':'")
                 (push (cons an (%parse-value p)) args))))
    (%parse-directives p)
    (when (and (%peek-tok p) (eq (gql-token-kind (%peek-tok p)) :lbrace))
      (setf sel (%parse-selection-set p)))
    (list :name name :alias alias :args (nreverse args) :sel sel)))

(defun %parse-selection (p)
  (cond
    ((%accept p :dot3)
     (%parse-directives p)
     (if (%accept-name p "on")
         (progn
           (%expect p :name "type condition")
           (%parse-directives p)
           (list :inline t :sel (%parse-selection-set p)))
         (list :spread (gql-token-value (%expect p :name "fragment name")))))
    (t (%parse-field p))))

(defun %parse-selection-set (p)
  (%expect p :lbrace "'{'")
  (let ((sel nil))
    (loop until (%accept p :rbrace)
          do (push (%parse-selection p) sel))
    (nreverse sel)))

(defun %parse-fragment (p)
  ;; the caller (gql-parse) has already consumed the "fragment" keyword
  (let ((name (gql-token-value (%expect p :name "fragment name"))))
    (%expect-name p "on")
    (let ((type (gql-token-value (%expect p :name "type condition"))))
      (%parse-directives p)
      (list :name name :type type :sel (%parse-selection-set p)))))

(defun %parse-operation (p)
  (let ((name nil) (vars nil) (sel nil))
    (%accept-name p "query")
    (let ((tok (%peek-tok p)))
      (when (and tok (eq (gql-token-kind tok) :name))
        (setf name (gql-token-value (%next-tok p)))))
    (when (%accept p :lparen)
      (loop until (%accept p :rparen)
            do (%expect p :dollar "variable definition")
               (%expect p :name "variable name")
               (%expect p :colon "':'")
               (%parse-type p)
               (let ((def :gql-null))
                 (when (%accept p :eq) (setf def (%parse-value p)))
                 (push (cons name def) vars))))
    (setf sel (%parse-selection-set p))
    (list :name name :vars (nreverse vars) :sel sel)))

(defun gql-parse (source)
  "Parse SOURCE into a document AST:
  (:document :fragments ((name . (:fragment ...)) ...)
             :operations ((:op ...) ...))."
  (let* ((tokens (gql-lex source))
         (p (gql-parser tokens))
         (fragments nil) (operations nil))
    (loop
      (let ((tok (%peek-tok p)))
        (cond ((null tok) (return))
              ((%accept-name p "query") (push (%parse-operation p) operations))
              ((%accept-name p "fragment") (push (%parse-fragment p) fragments))
              ((%accept p :lbrace)
               ;; anonymous operation: rewind and parse via %parse-operation
               (decf (gql-parser-pos p))
               (push (%parse-operation p) operations))
              (t (error "graphql: unexpected token")))))
    (unless operations (error "graphql: document has no query operation"))
    (list :fragments fragments :operations (nreverse operations))))

;;; ------------------------------------------------------------------
;;; executor

(defstruct (gql-exec
             (:constructor %make-gql-exec
                           (graph vars
                            &aux (nodes (make-hash-table :test #'equal))
                                 (edges (make-hash-table :test #'equal)))))
  graph vars nodes edges)

(defun %gql-jsonable (v)
  "Convert a Cypher value to a JSON-encodable Lisp structure."
  (cond ((cypher-null-p v) :json-null)
        ((eq v t) t)
        ((cypher-false-p v) :json-false)
        ((or (stringp v) (integerp v) (floatp v)) v)
        ((cypher-list-p v) (mapcar #'%gql-jsonable (cypher-list-elements v)))
        ((cypher-map-p v)
         (mapcar (lambda (p) (cons (car p) (%gql-jsonable (cdr p))))
                 (cypher-map-pairs v)))
        ((%node-p v) (list (cons "id" (getf v :id))
                           (cons "labels" (getf v :labels))))
        ((%rel-p v) (list (cons "id" (getf v :id))
                          (cons "type" (getf v :type))))
        (t (princ-to-string v))))

(defun %props-json (props)
  (mapcar (lambda (p) (cons (car p) (%gql-jsonable (cdr p)))) props))

(defun %collect-node (ex id)
  "Resolve node ID, remember it for extensions.graph, return (:gql-node . plist)."
  (let ((n (graph-node (gql-exec-graph ex) id)))
    (when n
      (setf (gethash id (gql-exec-nodes ex)) n)
      (cons :gql-node n))))

(defun %collect-rel (ex rid)
  "Resolve relationship RID, remember it, return (:gql-rel . plist)."
  (let ((r (graph-relationship (gql-exec-graph ex) rid)))
    (when r
      (setf (gethash rid (gql-exec-edges ex)) r)
      (cons :gql-rel r))))

(defun %resolve-arg (ex v)
  (cond ((and (consp v) (eq (car v) :var))
         (gethash (second v) (gql-exec-vars ex) :gql-null))
        ((and (consp v) (eq (car v) :obj))
         (mapcar (lambda (p) (cons (car p) (%resolve-arg ex (cdr p)))) (second v)))
        ((consp v) (mapcar (lambda (x) (%resolve-arg ex x)) v))
        (t v)))

(defun %field-arg (ex args name)
  (let ((p (assoc name args :test #'string=)))
    (and p (%resolve-arg ex (cdr p)))))

(defun %slice (list offset limit)
  (let ((rest (nthcdr (or offset 0) list)))
    (if limit
        (loop repeat limit for x in rest collect x)
        rest)))

(defun %gql-dir (s)
  (cond ((or (null s) (string= s "OUT")) :out)
        ((string= s "IN") :in)
        ((string= s "BOTH") :both)
        (t (error "graphql: bad direction ~s (expected OUT, IN or BOTH)" s))))

(defun %root-field (ex name args)
  (cond
    ((string= name "node")
     (let ((id (%field-arg ex args "id")))
       (if (and id (not (eq id :gql-null)))
           (%collect-node ex id)
           :gql-null)))
    ((string= name "nodes")
     (let* ((label (%field-arg ex args "label"))
            (limit (%field-arg ex args "limit"))
            (offset (%field-arg ex args "offset"))
            (ids (graph-scan-node-ids (gql-exec-graph ex) :label label)))
       (let ((m (mapcar (lambda (id) (%collect-node ex id)) (%slice ids offset limit))))
         (if m m #()))))
    ((string= name "relationship")
     (let ((id (%field-arg ex args "id")))
       (if (and id (not (eq id :gql-null)))
           (%collect-rel ex id)
           :gql-null)))
    ((string= name "relationships")
     (let* ((type (%field-arg ex args "type"))
            (limit (%field-arg ex args "limit"))
            (offset (%field-arg ex args "offset"))
            (ids (graph-scan-rel-ids (gql-exec-graph ex) :type type)))
       (let ((m (mapcar (lambda (rid) (%collect-rel ex rid)) (%slice ids offset limit))))
         (if m m #()))))
    ((string= name "__typename") "Query")
    (t (error "graphql: unknown field '~a' on Query" name))))

(defun %node-field (ex node name args)
  (cond
    ((string= name "id") (getf node :id))
    ((string= name "labels") (getf node :labels))
    ((string= name "properties") (%props-json (getf node :props)))
    ((string= name "relationships")
     (let* ((type (%field-arg ex args "type"))
            (dir (%gql-dir (%field-arg ex args "direction")))
            (limit (%field-arg ex args "limit"))
            (pairs (graph-expand (gql-exec-graph ex) (getf node :id) :dir dir :type type)))
       (let ((m (mapcar (lambda (pair) (%collect-rel ex (car pair)))
                         (%slice pairs nil limit))))
         (if m m #()))))
    ((string= name "out")
     (let* ((type (%field-arg ex args "type"))
            (limit (%field-arg ex args "limit"))
            (pairs (graph-expand (gql-exec-graph ex) (getf node :id) :dir :out :type type)))
       (let ((m (mapcar (lambda (pair) (%collect-rel ex (car pair)))
                         (%slice pairs nil limit))))
         (if m m #()))))
    ((string= name "in")
     (let* ((type (%field-arg ex args "type"))
            (limit (%field-arg ex args "limit"))
            (pairs (graph-expand (gql-exec-graph ex) (getf node :id) :dir :in :type type)))
       (let ((m (mapcar (lambda (pair) (%collect-rel ex (car pair)))
                         (%slice pairs nil limit))))
         (if m m #()))))
    ((string= name "__typename") "Node")
    (t (error "graphql: unknown field '~a' on Node" name))))

(defun %rel-field (ex rel name args)
  (declare (ignore args))
  (cond
    ((string= name "id") (getf rel :id))
    ((string= name "type") (getf rel :type))
    ((string= name "properties") (%props-json (getf rel :props)))
    ((or (string= name "from") (string= name "start"))
     (%collect-node ex (getf rel :start)))
    ((or (string= name "to") (string= name "end"))
     (%collect-node ex (getf rel :end)))
    ((string= name "__typename") "Relationship")
    (t (error "graphql: unknown field '~a' on Relationship" name))))

(defun %field-value (ex frags field obj-type obj)
  (destructuring-bind (&key name alias args sel) field
    (declare (ignore alias))
    (let ((v (case obj-type
               (:query (%root-field ex name args))
               (:node (%node-field ex obj name args))
               (:rel (%rel-field ex obj name args))
               (t (error "graphql: internal object type ~s" obj-type)))))
      (if sel
          (%exec-value ex frags v sel)
          ;; a node/relationship field selected without sub-fields yields its id
          (cond ((and (consp v) (eq (car v) :gql-node)) (getf (cdr v) :id))
                ((and (consp v) (eq (car v) :gql-rel)) (getf (cdr v) :id))
                (t v))))))

(defun %exec-value (ex frags v sel)
  (cond
    ((eq v :gql-null) :json-null)
    ((and (consp v) (eq (car v) :gql-node))
     (%exec-selection-set ex frags sel :node (cdr v)))
    ((and (consp v) (eq (car v) :gql-rel))
     (%exec-selection-set ex frags sel :rel (cdr v)))
    ((consp v)
     (let ((m (mapcar (lambda (x) (%exec-value ex frags x sel)) v)))
       (if m m #())))
    (t v)))

(defun %exec-selection-set (ex frags sel obj-type obj)
  "Execute SEL against OBJ; returns an alist of (out-key . jsonable)."
  (let ((out nil) (seen (make-hash-table :test #'equal)))
    (labels ((run (sel)
               (dolist (item sel)
                 (cond
                   ((and (consp item) (eq (car item) :spread))
                    (let ((f (find (second item) frags
                                   :key (lambda (fr) (getf fr :name))
                                   :test #'string=)))
                      (unless f (error "graphql: unknown fragment '~a'" (second item)))
                      (run (getf f :sel))))
                   ((and (consp item) (eq (car item) :inline))
                    (run (getf item :sel)))
                   (t
                    (let* ((name (getf item :name))
                           (alias (getf item :alias))
                           (key (or alias name)))
                      (unless (gethash key seen)
                        (setf (gethash key seen) t)
                        (push (cons key (%field-value ex frags item obj-type obj)) out))))))))
      (run sel)
      (nreverse out))))

(defun %exec-document (doc ex)
  (let* ((frags (getf doc :fragments))
         (op (first (getf doc :operations)))
         (sel (getf op :sel)))
    (%exec-selection-set ex frags sel :query nil)))

(defun %node-json (n)
  (list (cons "id" (getf n :id))
        (cons "labels" (getf n :labels))
        (cons "properties" (%props-json (getf n :props)))))

(defun %rel-json (r)
  (list (cons "id" (getf r :id))
        (cons "type" (getf r :type))
        (cons "start" (getf r :start))
        (cons "end" (getf r :end))
        (cons "properties" (%props-json (getf r :props)))))

(defun graphql-execute (source graph &key variables)
  "Execute GraphQL query SOURCE against GRAPH.  Returns an alist ready for
json-encode: ((\"data\" . data) (\"extensions\" . ((\"graph\" . ((\"nodes\" . ...)
(\"edges\" . ...)))))).  On parse/execution errors returns
((\"data\" . :json-null) (\"errors\" . (((\"message\" . ...))...)))."
  (handler-case
      (let* ((doc (gql-parse source))
             (ex (%make-gql-exec graph (or variables (make-hash-table :test #'equal))))
             (data (%exec-document doc ex)))
        ;; every edge endpoint must appear in the node set for rendering
        (loop for r being the hash-values of (gql-exec-edges ex)
              do (%collect-node ex (getf r :start))
                 (%collect-node ex (getf r :end)))
        (list (cons "data" data)
              (cons "extensions"
                    (list (cons "graph"
                                (list (cons "nodes"
                                            (let ((m (loop for n being the hash-values of (gql-exec-nodes ex)
                                                           collect (%node-json n))))
                                              (if m m #())))
                                      (cons "edges"
                                            (let ((m (loop for r being the hash-values of (gql-exec-edges ex)
                                                           collect (%rel-json r))))
                                              (if m m #())))))))))
    (error (e)
      (list (cons "data" :json-null)
            (cons "errors" (list (list (cons "message" (princ-to-string e)))))))))
