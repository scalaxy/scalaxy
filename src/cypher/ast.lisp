;;;; ast.lisp --- Cypher AST: constructors, accessors, canonical printer
;;;;
;;;; The AST is plain data: plists with keyword heads, variables as
;;;; case-sensitive symbols, expressions as nested lists.  Every stage
;;;; (parser output, planner input, plan output) is such data, so any
;;;; stage can be printed with AST-PRINT and inspected at the REPL
;;;; (the code-as-data axiom, plan section 6.2).

(in-package #:scalaxy)

(defun ast-var (name)
  "Variable for NAME (a symbol; case is preserved)."
  (intern name))

(defun ast-var-name (var)
  (symbol-name var))

;;; ------------------------------------------------------------------
;;; canonical printer

(defparameter *cypher-keywords*
  '("MATCH" "OPTIONAL" "WHERE" "RETURN" "WITH" "UNWIND" "AS" "DISTINCT"
    "CREATE" "MERGE" "SET" "REMOVE" "DELETE" "DETACH" "ORDER" "BY" "SKIP"
    "LIMIT" "UNION" "ALL" "ASC" "ASCENDING" "DESC" "DESCENDING"
    "AND" "OR" "XOR" "NOT" "IS" "NULL" "IN" "STARTS" "ENDS" "CONTAINS"
    "CASE" "WHEN" "THEN" "ELSE" "END" "TRUE" "FALSE" "COUNT"))

(defun %print-string (s)
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for ch across s
          do (case ch
               (#\' (write-string "\\'" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return (write-string "\\r" out))
               (#\Backspace (write-string "\\b" out))
               (#\Page (write-string "\\f" out))
               (t (write-char ch out))))
    (write-char #\' out)))

(defun %print-value (v)
  (cond
    ((cypher-null-p v) "null")
    ((eq v t) "true")
    ((cypher-false-p v) "false")
    ((stringp v) (%print-string v))
    ((integerp v) (format nil "~d" v))
    ((floatp v) (format nil "~f" v))
    ((typep v '(vector (unsigned-byte 8)))
     (format nil "blob(~s)" (hex-digest v)))
    ((cypher-empty-list-p v) "[]")
    ((cypher-map-p v)
     (format nil "{~{~a~^, ~}}"
             (mapcar (lambda (p) (format nil "~a: ~a" (car p) (%print-value (cdr p))))
                     (cypher-map-pairs v))))
    ((consp v)
     (format nil "[~{~a~^, ~}]" (mapcar #'%print-value v)))
    (t (princ-to-string v))))

(defun %print-expr (e)
  (etypecase e
    (symbol (ast-var-name e))
    (string (%print-string e))
    (number (format nil "~a" e))
    (cons
     (ecase (car e)
       (:lit (%print-value (second e)))
       (:param (format nil "$~a" (second e)))
       (:var (ast-var-name (second e)))
       (:prop (format nil "~a.~a" (%print-expr (getf (cdr e) :expr))
                      (getf (cdr e) :prop)))
       (:idx (format nil "~a[~a]" (%print-expr (getf (cdr e) :expr))
                     (%print-expr (getf (cdr e) :index))))
       (:bin (%print-bin (second e) (third e) (fourth e)))
       (:has-labels (format nil "~a~{~a~^:~}" (%print-expr (getf (cdr e) :expr))
                            (mapcar (lambda (l) (format nil ":~a" l))
                                    (getf (cdr e) :labels))))
       (:not (format nil "(NOT ~a)" (%print-expr (second e))))
       (:neg (format nil "(- ~a)" (%print-expr (second e))))
       (:is-null (format nil "(~a IS NULL)" (%print-expr (getf (cdr e) :expr))))
       (:is-not-null (format nil "(~a IS NOT NULL)" (%print-expr (getf (cdr e) :expr))))
       (:call (format nil "~a(~a~{~a~^, ~})"
                      (if (stringp (getf (cdr e) :fn))
                          (getf (cdr e) :fn)
                          (symbol-name (getf (cdr e) :fn)))
                      (if (getf (cdr e) :distinct) "DISTINCT " "")
                      (mapcar #'%print-expr (getf (cdr e) :args))))
       (:count-* "count(*)")
       (:list (format nil "[~{~a~^, ~}]" (mapcar #'%print-expr (getf (cdr e) :items))))
       (:map (format nil "{~{~a~^, ~}}"
                     (mapcar (lambda (p) (format nil "~a: ~a" (car p) (%print-expr (cdr p))))
                             (getf (cdr e) :pairs))))
       (:case (let ((parts nil))
               (when (getf (cdr e) :base)
                 (push (%print-expr (getf (cdr e) :base)) parts))
               (dolist (clause (getf (cdr e) :clauses))
                 (push (format nil "WHEN ~a THEN ~a"
                               (%print-expr (car clause)) (%print-expr (cdr clause)))
                       parts))
               (when (getf (cdr e) :else)
                 (push (format nil "ELSE ~a" (%print-expr (getf (cdr e) :else))) parts))
               (format nil "CASE ~{~a ~}END" (nreverse parts))))
       (:pcomp (format nil "[~a~a | ~a]"
                       (%print-chain (getf (cdr e) :chain))
                       (if (getf (cdr e) :where)
                           (format nil " WHERE ~a" (%print-expr (getf (cdr e) :where)))
                           "")
                       (%print-expr (getf (cdr e) :out))))
       (:comp (let ((var (ast-var-name (getf (cdr e) :var)))
                    (list (%print-expr (getf (cdr e) :list)))
                    (where (getf (cdr e) :where))
                    (out (getf (cdr e) :out)))
                (format nil "[~a IN ~a~a~a]"
                        var list
                        (if where (format nil " WHERE ~a" (%print-expr where)) "")
                        (if out (format nil " | ~a" (%print-expr out)) ""))))
       (:pred (format nil "~a(~a IN ~a WHERE ~a)"
                      (string-downcase (symbol-name (getf (cdr e) :kind)))
                      (ast-var-name (getf (cdr e) :var))
                      (%print-expr (getf (cdr e) :list))
                      (%print-expr (getf (cdr e) :pred))))
       (:exists (format nil "exists(~a)" (%print-chain (getf (cdr e) :chain))))
       (t (format nil "<?~a>" (car e)))))))

(defun %bin-prec (op)
  "Operator precedence (higher binds tighter) for minimal-paren printing."
  (ecase op
    (:or 1) (:xor 2) (:and 3)
    (:= 5) (:<> 5) (:< 5) (:> 5) (:<= 5) (:>= 5) (:=~ 5)
    (:in 5) (:starts 5) (:ends 5) (:contains 5)
    (:+ 6) (:- 6) (:* 7) (:/ 7) (:% 7) (:^ 8)))

(defun %print-bin (op a b)
  (let* ((opstr (ecase op
                  (:or "OR") (:xor "XOR") (:and "AND")
                  (:= "=") (:<> "<>") (:< "<") (:> ">") (:<= "<=") (:>= ">=")
                  (:+ "+") (:- "-") (:* "*") (:/ "/") (:% "%") (:^ "^")
                  (:=~ "=~") (:in "IN") (:starts "STARTS WITH")
                  (:ends "ENDS WITH") (:contains "CONTAINS")))
         (p (%bin-prec op))
         (left (if (and (consp a) (eq (car a) :bin)
                        (< (%bin-prec (second a)) p))
                   (format nil "(~a)" (%print-expr a))
                   (%print-expr a)))
         (right (if (and (consp b) (eq (car b) :bin)
                         (<= (%bin-prec (second b)) p))
                    (format nil "(~a)" (%print-expr b))
                    (%print-expr b))))
    (format nil "~a ~a ~a" left opstr right)))

(defun %print-props (props)
  (cond
    ((null props) "")
    ((and (consp props) (eq (car props) :param))
     (format nil " $~a" (second props)))
    (t (format nil "{~{~a~^, ~}}"
               (mapcar (lambda (p) (format nil "~a: ~a" (car p) (%print-expr (cdr p))))
                       props)))))

(defun %print-node (n)
  (let ((var (getf (cdr n) :var))
        (labels (getf (cdr n) :labels))
        (props (getf (cdr n) :props)))
    (format nil "(~a~{~a~^~}~a)"
            (if var (ast-var-name var) "")
            (mapcar (lambda (l) (format nil ":~a" l)) labels)
            (%print-props props))))

(defun %print-rel (r)
  (let ((var (getf (cdr r) :var))
        (type (getf (cdr r) :type))
        (dir (getf (cdr r) :dir))
        (props (getf (cdr r) :props)))
    (format nil "~a[~a~a~a]~a"
            (ecase dir (:in "<-") (:out "-") (:both "-"))
            (if var (ast-var-name var) "")
            (if type (format nil ":~a" type) "")
            (%print-props props)
            (ecase dir (:in "-") (:out "->") (:both "-")))))

(defun %print-chain (chain)
  (let ((parts nil))
    (dolist (el chain)
      (push (ecase (car el) (:node (%print-node el)) (:rel (%print-rel el))) parts))
    (format nil "~{~a~^~}" (nreverse parts))))

(defun %print-pattern (pattern)
  (format nil "~{~a~^, ~}" (mapcar #'%print-chain pattern)))

(defun %print-projection (items distinct?)
  (let ((head (if distinct? "DISTINCT " "")))
    (if (null items)
        (concatenate 'string head "*")
        (concatenate 'string head
                     (format nil "~{~a~^, ~}"
                             (mapcar (lambda (item)
                                       (let ((e (getf (cdr item) :expr))
                                             (as (getf (cdr item) :as)))
                                         (if as
                                             (format nil "~a AS ~a" (%print-expr e) (ast-var-name as))
                                             (%print-expr e))))
                                     items))))))

(defun %print-where (where)
  (if where (format nil " WHERE ~a" (%print-expr where)) ""))

(defun ast-print (form)
  "Print a Cypher AST form as a canonical query string (round-trips
through CYPHER-PARSE; law L14)."
  (etypecase form
    (cons
     (ecase (car form)
       (:query (format nil "~{~a~^~%~}" (mapcar #'ast-print (rest form))))
       (:match (format nil "MATCH ~a~a" (%print-pattern (getf (cdr form) :pattern))
                       (%print-where (getf (cdr form) :where))))
       (:optional-match (format nil "OPTIONAL MATCH ~a~a"
                                (%print-pattern (getf (cdr form) :pattern))
                                (%print-where (getf (cdr form) :where))))
       (:with (format nil "WITH ~a~a~a" (%print-projection (getf (cdr form) :items)
                                                            (getf (cdr form) :distinct))
                      (%print-where (getf (cdr form) :where))
                      (%print-order (getf (cdr form) :order))))
       (:return (format nil "RETURN ~a~a" (%print-projection (getf (cdr form) :items)
                                                             (getf (cdr form) :distinct))
                        (%print-order (getf (cdr form) :order))))
       (:unwind (format nil "UNWIND ~a AS ~a" (%print-expr (getf (cdr form) :expr))
                        (ast-var-name (getf (cdr form) :var))))
       (:create (format nil "CREATE ~a" (%print-pattern (getf (cdr form) :pattern))))
       (:merge (format nil "MERGE ~a" (%print-pattern (getf (cdr form) :pattern))))
       (:set (format nil "SET ~{~a~^, ~}" (mapcar #'%print-set-item (getf (cdr form) :items))))
       (:remove (format nil "REMOVE ~{~a~^, ~}"
                        (mapcar #'%print-remove-item (getf (cdr form) :items))))
       (:delete (format nil "~aDELETE ~{~a~^, ~}"
                        (if (getf (cdr form) :detach) "DETACH " "")
                        (mapcar #'%print-expr (getf (cdr form) :items))))
       (:order (format nil "ORDER BY ~{~a~^, ~}"
                       (mapcar #'%print-order-spec (getf (cdr form) :items))))
       (:skip (format nil "SKIP ~a" (%print-expr (getf (cdr form) :expr))))
       (:limit (format nil "LIMIT ~a" (%print-expr (getf (cdr form) :expr))))
       (:union (let ((qs (getf (cdr form) :queries)) (all? (getf (cdr form) :all)))
                (with-output-to-string (out)
                  (loop for q in qs for first = t then nil
                        do (unless first
                             (format out "~%UNION~@[ ALL~]~%" all?))
                           (format out "~a" (ast-print q))))))
       (:expr (%print-expr (second form)))
       (t (format nil "<?~a>" (car form)))))
    (symbol (ast-var-name form))))

(defun %print-set-item (item)
  (ecase (car item)
    (:set-prop (format nil "~a.~a = ~a" (ast-var-name (getf (cdr item) :var))
                       (getf (cdr item) :prop)
                       (%print-expr (getf (cdr item) :expr))))
    (:add-prop (format nil "~a.~a += ~a" (ast-var-name (getf (cdr item) :var))
                       (getf (cdr item) :prop)
                       (%print-expr (getf (cdr item) :expr))))
    (:set-var (format nil "~a = ~a" (ast-var-name (getf (cdr item) :var))
                      (%print-expr (getf (cdr item) :expr))))
    (:set-label (format nil "~a:~a" (ast-var-name (getf (cdr item) :var))
                        (getf (cdr item) :label)))))

(defun %print-remove-item (item)
  (ecase (car item)
    (:remove-prop (format nil "~a.~a" (ast-var-name (getf (cdr item) :var))
                          (getf (cdr item) :prop)))
    (:remove-label (format nil "~a:~a" (ast-var-name (getf (cdr item) :var))
                           (getf (cdr item) :label)))))

(defun %print-order-spec (spec)
  (let ((e (%print-expr (getf (cdr spec) :expr)))
        (desc (getf (cdr spec) :desc)))
    (if desc (concatenate 'string e " DESC") e)))

(defun %print-order (specs)
  (if specs
      (format nil " ORDER BY ~{~a~^, ~}" (mapcar #'%print-order-spec specs))
      ""))
