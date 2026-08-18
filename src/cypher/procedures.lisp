;;;; procedures.lisp --- stored-procedure registry for the CALL clause
;;;;
;;;; Procedures are pure functions from input arguments to output rows,
;;;; declared either by the application (register-procedure) or by the
;;;; openCypher TCK harness ("there exists a procedure ..." steps).  A
;;;; procedure is a table of rows: input columns are matched against the
;;;; call arguments (numeric cross-type equality, null == null); output
;;;; columns are projected.  A procedure with no output columns acts as a
;;;; pass-through in queries (each input row survives unchanged); called
;;;; standalone it yields nothing.

(in-package #:scalaxy)

(defvar *procedures* (make-hash-table :test #'equal)
  "Registry: procedure name (string, lowercase) -> procedure plist
(:name :inputs ((name . type) ...) :outputs ((name . type) ...) :rows
((name . value) ...) ...).")

(defun register-procedure (name inputs outputs rows)
  "Register a procedure: NAME (string), INPUTS/OUTPUTS as lists of
(name . type) pairs, ROWS as a list of alists covering input and output
columns.  Returns the procedure plist."
  (setf (gethash (string-downcase name) *procedures*)
        (list :name (string-downcase name)
              :inputs inputs :outputs outputs :rows rows)))

(defun %proc-lookup (name)
  "The procedure plist for NAME (string), or NIL."
  (gethash (string-downcase name) *procedures*))

(defun %proc-value-eq (a b)
  "Equality used to match procedure rows: numeric cross-type
(42 == 42.0), null == null, otherwise ordinary equality."
  (cond ((and (cypher-null-p a) (cypher-null-p b)) t)
        ((and (numberp a) (numberp b)) (= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (equal a b))))

(defun %procedure-rows (proc args)
  "Output rows of PROC for ARGS (alist of (input-name . value)).
Rows whose input columns match ARGS are projected onto the output
columns; with no output columns the result is NIL (the caller decides
pass-through behaviour)."
  (let ((inputs (getf proc :inputs))
        (outputs (getf proc :outputs)))
    (if (null outputs)
        nil
        (loop for row in (getf proc :rows)
              when (every (lambda (in)
                            (let ((name (car in)))
                              (%proc-value-eq
                               (cdr (assoc name row :test #'string=))
                               (cdr (assoc name args :test #'string=)))))
                          inputs)
                collect (loop for (name . type) in outputs
                              collect (cons (ast-var name)
                                            (cdr (assoc name row :test #'string=))))))))
