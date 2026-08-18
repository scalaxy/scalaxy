;;;; conditions.lisp --- Cypher error taxonomy (openCypher 9)
;;;;
;;;; The error hierarchy follows the openCypher specification (section 4
;;;; of openCypher9.pdf): SyntaxError / TypeError / ArgumentError /
;;;; EntityNotFound, each with the specification's named subtypes.  The
;;;; TCK compares errors by these subtype names, so CYPHER-ERROR-KIND
;;;; returns exactly the spec string (e.g. "UnexpectedSyntax").

(in-package #:scalaxy)

(define-condition cypher-error (error)
  ((query :initarg :query :initform nil :reader cypher-error-query)
   (detail :initarg :detail :initform nil :reader cypher-error-detail)
   (kind :initarg :kind :initform "CypherError" :reader cypher-error-kind))
  (:report (lambda (c s)
             (format s "~a~@[ [~a]~]~@[ in query ~s~]"
                     (cypher-error-kind c)
                     (cypher-error-detail c)
                     (cypher-error-query c)))))

(define-condition cypher-syntax-error (cypher-error) ())
(define-condition cypher-type-error (cypher-error) ())
(define-condition cypher-argument-error (cypher-error) ())
(define-condition cypher-entity-not-found (cypher-error) ())
(define-condition cypher-procedure-error (cypher-error) ())
(define-condition cypher-parameter-missing (cypher-error) ())

(defparameter *cypher-error-classes* (make-hash-table :test #'equal))

(defun define-cypher-error (kind parent)
  (let* ((suffix (substitute #\- #\_ (string-upcase kind)))
         (class (intern (format nil "CYPHER-~a" suffix))))
    (eval `(define-condition ,class (,parent) ()))
    (setf (gethash (string-downcase kind) *cypher-error-classes*) class)
    class))

;;; SyntaxError subtypes (openCypher 9 section 4.3.1)
(define-cypher-error "InvalidArgumentType"           'cypher-syntax-error)
(define-cypher-error "VariableAlreadyBound"          'cypher-syntax-error)
(define-cypher-error "UndefinedVariable"             'cypher-syntax-error)
(define-cypher-error "VariableTypeConflict"          'cypher-syntax-error)
(define-cypher-error "InvalidNumberLiteral"          'cypher-syntax-error)
(define-cypher-error "IntegerOverflow"               'cypher-syntax-error)
(define-cypher-error "UnexpectedSyntax"              'cypher-syntax-error)
(define-cypher-error "InvalidAggregation"            'cypher-syntax-error)
(define-cypher-error "AmbiguousAggregationExpression" 'cypher-syntax-error)
(define-cypher-error "InvalidClauseComposition"      'cypher-syntax-error)
(define-cypher-error "NonConstantExpression"         'cypher-syntax-error)
(define-cypher-error "InvalidParameterUse"           'cypher-syntax-error)
(define-cypher-error "InvalidNumberOfArguments"      'cypher-syntax-error)
(define-cypher-error "NoSingleRelationshipType"      'cypher-syntax-error)
(define-cypher-error "RequiresDirectedRelationship"  'cypher-syntax-error)
(define-cypher-error "InvalidRelationshipPattern"    'cypher-syntax-error)
(define-cypher-error "RelationshipUniquenessViolation" 'cypher-syntax-error)
(define-cypher-error "InvalidDelete"                 'cypher-syntax-error)
(define-cypher-error "CreatingVarLength"             'cypher-syntax-error)
(define-cypher-error "DifferentColumnsInUnion"       'cypher-syntax-error)
(define-cypher-error "ColumnNameConflict"            'cypher-syntax-error)
(define-cypher-error "NoExpressionAlias"             'cypher-syntax-error)
(define-cypher-error "NestedAggregation"             'cypher-syntax-error)
(define-cypher-error "AmbiguousAggregationExpression" 'cypher-syntax-error)
(define-cypher-error "NegativeIntegerArgument"        'cypher-syntax-error)
(define-cypher-error "NoVariablesInScope"             'cypher-syntax-error)
(define-cypher-error "InvalidArgumentPassingMode"    'cypher-syntax-error)

;;; TypeError subtypes
(define-cypher-error "InvalidArgumentValue"          'cypher-type-error)
(define-cypher-error "MapElementAccessByNonString"   'cypher-type-error)
;;; InvalidArgumentType exists under both SyntaxError (compile-time)
;;; and TypeError (runtime) families (openCypher TCK).
(define-cypher-error "InvalidArgumentType"           'cypher-type-error)

;;; ArgumentError subtypes
(define-cypher-error "NumberOutOfRange"              'cypher-argument-error)

;;; EntityNotFound subtypes
(define-cypher-error "DeletedEntityAccess"           'cypher-entity-not-found)

;;; ProcedureError / ParameterMissing families (CALL clause)
(define-cypher-error "ProcedureNotFound"             'cypher-procedure-error)
(define-cypher-error "MissingParameter"              'cypher-parameter-missing)

(defun cypher-signal (kind &key (query nil) (detail nil) (family nil))
  "Signal the Cypher error of KIND (spec name, e.g. \"UnexpectedSyntax\")
or a generic error of the matching family.  FAMILY selects among the
families that share KIND (e.g. InvalidArgumentType under SyntaxError
vs TypeError); the class registry records the most recently defined
class per kind."
  (let* ((key (string-downcase kind))
         (class (cond
                  ((string-equal kind "InvalidArgumentType")
                   ;; shared kind: SyntaxError by default, TypeError when
                   ;; signalled with :family "TypeError" (runtime typing)
                   (if (and family (string-equal family "TypeError"))
                       'cypher-type-error
                       'cypher-syntax-error))
                  (t
                   (or (gethash key *cypher-error-classes*)
                       (cond ((member kind '("SyntaxError") :test #'string-equal)
                              'cypher-syntax-error)
                             ((string-equal kind "TypeError") 'cypher-type-error)
                             ((string-equal kind "ArgumentError") 'cypher-argument-error)
                             ((string-equal kind "EntityNotFound") 'cypher-entity-not-found)
                             (t 'cypher-error)))))))
    (error class :kind kind :query query :detail detail)))
