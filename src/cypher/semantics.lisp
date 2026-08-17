;;;; semantics.lisp --- static semantic checks (openCypher error taxonomy)
;;;;
;;;; Pure: (CYPHER-CHECK ast) -> ast, or signals the spec's syntax
;;;; errors: UndefinedVariable, VariableAlreadyBound,
;;;; VariableTypeConflict, InvalidAggregation, NonConstantExpression,
;;;; DifferentColumnsInUnion, InvalidClauseComposition.

(in-package #:scalaxy)

(defun %in-scope (var scope)
  (assoc var scope))

(defun %check-var (var scope)
  (when (and var (not (%in-scope var scope)))
    (cypher-signal "UndefinedVariable" :detail (symbol-name var))))

(defparameter *known-scalar-fns*
  '("abs" "ceil" "coalesce" "endNode" "exists" "floor" "head" "id"
    "keys" "lTrim" "labels" "last" "left" "length" "nodes"
    "properties" "rTrim" "rand" "range" "relationships" "replace"
    "reverse" "right" "round" "sign" "size" "split" "sqrt"
    "startNode" "substring" "tail" "toBoolean" "toFloat" "toInteger"
    "toLower" "toString" "toUpper" "trim" "type"))

(defun %entity-kind-of (expr scope)
  "Static entity kind of EXPR (:node/:rel/:path/:other) or nil if unknown."
  (cond
    ((symbolp expr)
     (let ((b (%in-scope expr scope))) (and b (cdr b))))
    ((and (consp expr) (eq (car expr) :var))
     (let ((b (%in-scope (second expr) scope))) (and b (cdr b))))
    (t nil)))

(defun %check-expr-types (expr scope)
  "openCypher compile-time entity type checks: functions that require a
specific entity kind and property access, so errors like
length() on a node surface as SyntaxError: InvalidArgumentType."
  (cond
    ((atom expr) nil)
    ((eq (car expr) :prop)
     (let ((kind (%entity-kind-of (getf (cdr expr) :expr) scope)))
       (when (eq kind :path)
         (cypher-signal "InvalidArgumentType"
                        :detail "property access on a path")))
     (%check-expr-types (getf (cdr expr) :expr) scope))
    ((eq (car expr) :call)
     (let ((fn (getf (cdr expr) :fn))
           (args (getf (cdr expr) :args)))
       (unless (or (%aggregate-fn-p fn)
                   (member fn *known-scalar-fns* :test #'string-equal))
         (cypher-signal "UnknownFunction" :detail fn))
       (dolist (a args) (%check-expr-types a scope))
       (when (and (= (length args) 1)
                  (member fn '("length" "size" "type" "labels" "keys"
                               "properties" "id" "startNode" "endNode")
                          :test #'string-equal))
         (let ((kind (%entity-kind-of (first args) scope)))
           (flet ((bad ()
                    (cypher-signal "InvalidArgumentType"
                                   :detail (format nil "~a() argument" fn))))
             (cond
               ((string-equal fn "length")
                (when (member kind '(:node :rel)) (bad)))
               ((string-equal fn "size")
                (when (eq kind :path) (bad)))
               ((string-equal fn "type")
                (when (member kind '(:node :path)) (bad)))
               ((string-equal fn "labels")
                (when (member kind '(:rel :path)) (bad)))
               ((string-equal fn "keys")
                (when (eq kind :path) (bad)))
               ((string-equal fn "properties")
                (when (eq kind :path) (bad)))
               ((string-equal fn "id")
                (when (member kind '(:rel :path)) (bad)))
               ((string-equal fn "startNode")
                (when (member kind '(:node :path)) (bad)))
               ((string-equal fn "endNode")
                (when (member kind '(:node :path)) (bad)))))))))
    ((eq (car expr) :idx)
     (%check-expr-types (getf (cdr expr) :expr) scope)
     (%check-expr-types (getf (cdr expr) :index) scope))
    ((eq (car expr) :bin)
     (%check-expr-types (third expr) scope)
     (%check-expr-types (fourth expr) scope))
    ((eq (car expr) :un)
     (%check-expr-types (second expr) scope))
    ((eq (car expr) :has-labels)
     (%check-expr-types (getf (cdr expr) :expr) scope))
    ((eq (car expr) :comp)
     (when (or (and (getf (cdr expr) :where)
                    (expr-has-aggregate (getf (cdr expr) :where)))
               (and (getf (cdr expr) :out)
                    (expr-has-aggregate (getf (cdr expr) :out))))
       (cypher-signal "InvalidAggregation"
                      :detail "aggregation in list comprehension"))
     (%check-expr-types (getf (cdr expr) :list) scope)
     (%check-expr-types (getf (cdr expr) :where) scope)
     (%check-expr-types (getf (cdr expr) :out) scope))
    (t nil)))

(defun %elem-kind (el)
  (ecase (car el) (:node :node) (:rel :rel)))

(defun %bind-pattern-var (scope var kind &key (allow-value nil))
  (let ((existing (%in-scope var scope)))
    (cond
      ((null existing) (acons var kind scope))
      ((eq (cdr existing) kind) scope)
      ;; a variable with unknown type (null literal, parameter) may be
      ;; anchored as a node/rel/path and simply fails to match at runtime
      ((eq (cdr existing) :other) (acons var kind scope))
      ;; a value (e.g. a list of relationships) may anchor a var-length
      ;; relationship variable: MATCH (first)-[rs*]->(second)
      ((and allow-value (eq (cdr existing) :value)) (acons var kind scope))
      ((eq kind :path)
       (cypher-signal "VariableAlreadyBound" :detail (symbol-name var)))
      (t (cypher-signal "VariableTypeConflict"
                        :detail (symbol-name var))))))

(defun %expr-vars (expr)
  "All variables referenced by EXPR."
  (cond
    ((symbolp expr) (list expr))
    ((atom expr) nil)
    ;; map pair (key . value) with a symbol value
    ((and (consp expr) (atom (cdr expr)))
     (if (and (cdr expr) (symbolp (cdr expr))) (list (cdr expr)) nil))
    ((eq (car expr) :var) (list (second expr)))
    ((member (car expr) '(:lit :param :count-*)) nil)
    ((eq (car expr) :prop) (%expr-vars (getf (cdr expr) :expr)))
    ((eq (car expr) :idx)
     (append (%expr-vars (getf (cdr expr) :expr))
             (%expr-vars (getf (cdr expr) :index))))
    ((eq (car expr) :bin)
     (append (%expr-vars (third expr)) (%expr-vars (fourth expr))))
    ((eq (car expr) :has-labels)
     (%expr-vars (getf (cdr expr) :expr)))
    ((eq (car expr) :comp)
     (remove (getf (cdr expr) :var)
             (append (%expr-vars (getf (cdr expr) :list))
                     (%expr-vars (getf (cdr expr) :where))
                     (%expr-vars (getf (cdr expr) :out)))))
    ((eq (car expr) :pcomp)
     (let ((chain-vars (%pattern-vars (list (getf (cdr expr) :chain)))))
       (remove-if (lambda (v) (member v chain-vars))
                  (append (%expr-vars (getf (cdr expr) :where))
                          (%expr-vars (getf (cdr expr) :out))))))
    ((eq (car expr) :pred)
     (remove (getf (cdr expr) :var)
             (append (%expr-vars (getf (cdr expr) :list))
                     (%expr-vars (getf (cdr expr) :pred)))))
    ((eq (car expr) :call)
     (loop for a in (getf (cdr expr) :args)
           append (if (symbolp a) (list a) (%expr-vars a))))
    ((eq (car expr) :exists) nil)
    ((eq (car expr) :exists-sub) nil)
    (t (loop for x in (rest expr)
             when (listp x) append (%expr-vars x)))))

(defun %pattern-chain-vars (chain)
  "All variables bound by a pattern chain (nodes and relationships)."
  (let ((vars nil))
    (dolist (el chain)
      (let ((v (getf (cdr el) :var)))
        (when (and v (not (member v vars)))
          (push v vars))))
    (nreverse vars)))

(defun %check-exists-pattern (expr scope)
  "Check a bare pattern predicate (WHERE (chain) / EXISTS (chain)):
every pattern variable must be pre-bound (UndefinedVariable), and a
single-node pattern is invalid (InvalidArgumentType)."
  (let ((chain (getf (cdr expr) :chain)))
    (dolist (v (%pattern-chain-vars chain))
      (%check-var v scope))
    (when (null (rest chain))
      (cypher-signal "InvalidArgumentType" :detail "self-pattern predicate"))))

(defun %check-exists-sub (expr scope)
  "Check an EXISTS { <clauses> } subquery: MATCH patterns may
reference outer-scope variables; subquery variables are local.  A
bare-pattern subquery is a pattern predicate."
  (let ((chain (getf (cdr expr) :chain)))
    (if chain
        (dolist (v (%pattern-chain-vars chain))
          (unless (%in-scope v scope)
            (cypher-signal "UndefinedVariable" :detail (symbol-name v))))
        (%check-clauses (getf (cdr expr) :clauses) scope))))

(defun %check-expr-vars (expr scope)
  (cond
    ((and (consp expr) (eq (car expr) :exists))
     (%check-exists-pattern expr scope))
    ((and (consp expr) (eq (car expr) :exists-sub))
     (%check-exists-sub expr scope))
    ((and (consp expr) (member (car expr) '(:not :bin)))
     (dolist (x (rest expr))
       (cond ((consp x) (%check-expr-vars x scope))
             ((and x (symbolp x) (not (keywordp x)))
              (%check-var x scope)))))
    (t (dolist (v (%expr-vars expr))
         (%check-var v scope)))))

(defun %check-constant (expr scope)
  "SKIP/LIMIT must be constant (no variables, per spec NonConstantExpression)."
  (when (%expr-vars expr)
    (cypher-signal "NonConstantExpression" :detail "SKIP/LIMIT must be constant")))

(defun %check-pattern (pattern scope &key (bind t) (fresh nil))
  "Check MATCH/CREATE pattern variables; returns the extended scope.
When FRESH is true (CREATE/MERGE), a variable that is already bound is
a VariableAlreadyBound error."
  (let ((s scope)
        (rel-vars nil))
    (dolist (chain pattern)
      (let ((in-path? (eq (car chain) :path-var)))
        (when in-path?
          (when bind
            (if (%in-scope (second chain) s)
                (cypher-signal "VariableAlreadyBound"
                               :detail (symbol-name (second chain)))
                (setf s (%bind-pattern-var s (second chain) :path))))
          (setf chain (cddr chain)))
        (dolist (el chain)
          (when (eq (car el) :rel)
            (let ((v (getf (cdr el) :var)))
              (when v
                (if (member v rel-vars)
                    (cypher-signal "RelationshipUniquenessViolation"
                                   :detail (symbol-name v))
                    (push v rel-vars))))))
        (dolist (el chain)
          (let ((var (getf (cdr el) :var)))
            (when var
              (when (and fresh (%in-scope var s))
                (cypher-signal "VariableAlreadyBound" :detail (symbol-name var)))
              (if (and in-path? (eq (cdr (%in-scope var s)) :path))
                  (cypher-signal "VariableAlreadyBound" :detail (symbol-name var))
                  (setf s (%bind-pattern-var s var (%elem-kind el)
                                             :allow-value (and (eq (car el) :rel)
                                                               (getf (cdr el) :var-length))))))
            (dolist (p (getf (cdr el) :props))
              (let ((expr (cdr p)))
                (unless (atom expr)
                  (dolist (v (%expr-vars expr))
                    (unless (%in-scope v s)
                      (%check-var v scope))))))))))
    s))

(defun %check-create-pattern (pattern scope &key (require-directed nil))
  "CREATE/MERGE pattern check.  A node variable bound before this clause
is legal only as a relationship anchor; a variable created within this
clause cannot be reused (VariableAlreadyBound), a standalone bound node
pattern would create a second entity (VariableAlreadyBound), and a
relationship variable that is already bound is an error.  When
REQUIRE-DIRECTED (CREATE), relationships must be directed."
  (let ((s scope)
        (created nil)
        (created-rels nil))
    (dolist (chain pattern)
      (let ((elements (if (eq (car chain) :path-var) (cddr chain) chain)))
        ;; a path variable is bound to the matched/created path
        (when (eq (car chain) :path-var)
          (when (%in-scope (second chain) s)
            (cypher-signal "VariableAlreadyBound" :detail (symbol-name (second chain))))
          (setf s (acons (second chain) :path s)))
        ;; relationship variables: always create fresh
        (dolist (el elements)
          (when (eq (car el) :rel)
            (when (and (getf (cdr el) :var)
                       (%in-scope (getf (cdr el) :var) s))
              (cypher-signal "VariableAlreadyBound"
                             :detail (symbol-name (getf (cdr el) :var))))
            (when (getf (cdr el) :var-length)
              (cypher-signal "CreatingVarLength" :detail "variable-length relationships cannot be created"))
            (when (or (null (getf (cdr el) :type))
                      (> (length (getf (cdr el) :types)) 1))
              (cypher-signal "NoSingleRelationshipType"
                             :detail "a relationship must have exactly one type"))
            (when (and require-directed (eq (getf (cdr el) :dir) :both))
              ;; CREATE relationships must have a single direction
              (cypher-signal "RequiresDirectedRelationship"
                             :detail "a created relationship must have a direction"))
            (when (and (consp (getf (cdr el) :props))
                       (eq (car (getf (cdr el) :props)) :param))
              (cypher-signal "InvalidParameterUse"
                             :detail "parameter is not allowed as a relationship pattern"))
            (let ((v (getf (cdr el) :var)))
              (when v
                (when (or (%in-scope v s) (member v created))
                  (cypher-signal "VariableAlreadyBound" :detail (symbol-name v)))
                (push v created)
                (push v created-rels)))))
        ;; node variables
        (let ((single? (= (length elements) 1)))
          (dolist (el elements)
            (when (eq (car el) :node)
              (let ((v (getf (cdr el) :var)))
                (when v
                  (cond
                    ((and single? (or (%in-scope v s) (member v created)))
                     ;; standalone node pattern would create a second entity
                     (cypher-signal "VariableAlreadyBound" :detail (symbol-name v)))
                    ((and (not single?) (or (%in-scope v s) (member v created)))
                     ;; reuse as an anchor: legal only for a bare node
                     ;; pattern (no new labels/properties)
                     (when (or (getf (cdr el) :labels) (getf (cdr el) :props))
                       (cypher-signal "VariableAlreadyBound" :detail (symbol-name v))))
                    (t (push v created)))))
              ;; property expressions may reference pre-scope variables
              ;; or variables created earlier in this clause
              (dolist (pr (getf (cdr el) :props))
                (when (and (consp pr) (not (eq (car pr) :empty-props)))
                (let ((expr (cdr pr)))
                  (cond
                    ((and (symbolp expr) (not (keywordp expr)))
                     (unless (or (%in-scope expr s) (member expr created))
                       (%check-var expr scope)))
                    ((consp expr)
                     (dolist (v (%expr-vars expr))
                       (unless (or (%in-scope v s) (member v created))
                         (%check-var v scope)))))))))))))
    ;; created entities are bound for subsequent clauses
    (dolist (v created)
      (setf s (acons v (if (member v created-rels) :rel :node) s)))
    s))

(defun %check-projection (clause scope)
  (let ((items (getf (cdr clause) :items))
        (where (getf (cdr clause) :where))
        (new-scope nil))
    (when where
      (when (expr-has-aggregate where)
        (cypher-signal "InvalidAggregation"
                       :detail "aggregation is not allowed in WHERE"))
      (if (eq (car clause) :with)
          ;; WITH WHERE may reference input variables or the projected
          ;; aliases (the WHERE is applied after the projection)
          (let ((aliases (mapcar (lambda (i)
                                   (or (getf (cdr i) :as)
                                       (ast-var (ast-print
                                                 (list :expr (getf (cdr i) :expr))))))
                                 items)))
            (dolist (v (%expr-vars where))
              (unless (or (%in-scope v scope) (member v aliases))
                (%check-var v scope))))
          (%check-expr-vars where scope)))
    (if (null items)
        ;; projection *
        (progn
          (when (and (eq (car clause) :return) (null scope))
            (cypher-signal "NoVariablesInScope"
                           :detail "RETURN * with no variables in scope"))
          (setf new-scope scope))
        (let ((has-agg? (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr)))
                              items))
              (order (getf (cdr clause) :order)))
          (when (or has-agg?
                    (some (lambda (s) (expr-has-aggregate (getf (cdr s) :expr)))
                          order))
            (%check-agg-scope items order (car clause) scope))
          (dolist (item items)
            (let ((expr (getf (cdr item) :expr))
                  (as (getf (cdr item) :as)))
              (%check-expr-types expr scope)
              (cond
                ((and has-agg? (not (expr-has-aggregate expr)))
                 (when (expr-has-aggregate expr)
                   (cypher-signal "InvalidAggregation"
                                  :detail "aggregation in grouping key"))
                 (%check-expr-vars expr scope))
                ((expr-has-aggregate expr)
                 (when (and (consp expr) (eq (car expr) :call)
                            (%aggregate-fn-p (getf (cdr expr) :fn))
                            (some #'expr-has-aggregate (getf (cdr expr) :args)))
                   (cypher-signal "NestedAggregation" :detail "nested aggregation"))
                 (%check-expr-vars expr scope))
                (t (%check-expr-vars expr scope)))
              (when (and (eq (car clause) :with)
                         (null as)
                         (not (symbolp expr)))
                (cypher-signal "NoExpressionAlias"
                               :detail (format nil "expression ~a needs an alias"
                                               (ast-print (list :expr expr)))))
              (let ((alias (or as (ast-var (ast-print (list :expr expr))))))
                (when (%in-scope alias new-scope)
                  (cypher-signal "ColumnNameConflict" :detail (symbol-name alias)))
                (let ((kind (cond
                              ((symbolp expr)
                               (let ((old (%in-scope expr scope)))
                                 (if old (cdr old) :other)))
                              ;; a null literal has unknown type; any other
                              ;; value (literals, lists, maps, calls) is a
                              ;; concrete non-entity
                              ((and (consp expr) (eq (car expr) :lit)
                                    (%tv-null (second expr)))
                               :other)
                              (t :value))))
                  (push (cons alias kind) new-scope)))))
          (setf new-scope (nreverse new-scope))))
    ;; ORDER BY (non-aggregating): RETURN may only reference projected
    ;; columns; WITH may reference the input scope or projected aliases
    ;; (except after DISTINCT, where only the projected columns remain)
    (let ((order (getf (cdr clause) :order))
          (distinct? (getf (cdr clause) :distinct)))
      (when (and order
                 (not (some (lambda (s) (expr-has-aggregate (getf (cdr s) :expr)))
                            order))
                 (not (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr)))
                            items)))
        (dolist (spec order)
          (let ((e (getf (cdr spec) :expr)))
            ;; the whole expression may be a projected column
            ;; (ORDER BY a.name after WITH DISTINCT a.name AS name)
            (unless (member e (mapcar (lambda (i) (getf (cdr i) :expr)) items)
                            :test #'equal)
              (dolist (v (%expr-vars e))
                (unless (or (%in-scope v new-scope)  ; projected aliases
                            (and (not distinct?)
                                 (%in-scope v scope)))  ; input-scope variables
                  (cypher-signal "UndefinedVariable" :detail (symbol-name v))))))))
    new-scope)))


(defun %agg-calls-of (expr)
  "All aggregate calls (:call with an aggregate fn, :count-*) in EXPR."
  (cond
    ((atom expr) nil)
    ((eq (car expr) :count-*) (list expr))
    ((and (eq (car expr) :call) (%aggregate-fn-p (getf (cdr expr) :fn)))
     (list expr))
    (t (loop for c in (%expr-direct-parts expr) append (%agg-calls-of c)))))

(defun %expr-subexprs (expr)
  "All proper subexpressions of EXPR (cons cells inside it)."
  (loop for c in (cdr expr)
        when (consp c) collect c
        append (when (consp c) (%expr-subexprs c))))

(defun %agg-outer-subexprs (expr)
  "The maximal variable-bearing subexpressions of EXPR that contain no
aggregate calls (leaves of the aggregate-free skeleton)."
  (cond
    ((atom expr) nil)
    ((eq (car expr) :count-*) nil)
    ((and (eq (car expr) :call) (%aggregate-fn-p (getf (cdr expr) :fn))) nil)
    ((eq (car expr) :comp)
     ;; list comprehension: the bound variable is local; drop leaves
     ;; whose only variables are the bound variable
     (let ((var (getf (cdr expr) :var)))
       (remove-if (lambda (leaf)
                    (and leaf (null (remove var (%expr-vars leaf)))))
                  (append (%agg-outer-subexprs (getf (cdr expr) :list))
                          (when (getf (cdr expr) :where)
                            (%agg-outer-subexprs (getf (cdr expr) :where)))
                          (when (getf (cdr expr) :out)
                            (%agg-outer-subexprs (getf (cdr expr) :out)))))))
    ((eq (car expr) :pred)
     (remove (getf (cdr expr) :var)
             (append (%agg-outer-subexprs (getf (cdr expr) :list))
                     (%agg-outer-subexprs (getf (cdr expr) :pred)))))
    ((or (expr-has-aggregate expr)
         (member (car expr) '(:bin :neg :not :is-null :is-not-null :case)))
     (loop for c in (cdr expr)
           if (consp c) append (%agg-outer-subexprs c)
           else if (and c (symbolp c) (not (keywordp c))) collect c))
    (t (list expr))))

(defun %check-agg-scope (items order kind &optional scope)
  "openCypher aggregation scope rules for projection items and their
ORDER BY subclause."
  (let ((key-exprs (loop for item in items
                         unless (expr-has-aggregate (getf (cdr item) :expr))
                           collect (getf (cdr item) :expr)))
        (projected-exprs
          (loop for item in items collect (getf (cdr item) :expr)))
        (projected-names
          (loop for item in items
                collect (or (getf (cdr item) :as)
                            (ast-var (ast-print
                                      (list :expr (getf (cdr item) :expr)))))))
        (has-agg? (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr)))
                        items)))
    (labels ((check-leaves (expr)
               (dolist (leaf (%agg-outer-subexprs expr))
                 (when (%expr-vars leaf)
                   (unless (member leaf key-exprs :test #'equal)
                     (cypher-signal "AmbiguousAggregationExpression"
                                    :detail (ast-print (list :expr leaf)))))))
             (check-order-leaves (expr)
               ;; ORDER BY leaves in an aggregating projection must be
               ;; projected keys; a leaf inside a key is ambiguous; an
               ;; unrelated bound variable is undefined here
               (dolist (leaf (%agg-outer-subexprs expr))
                 (when (%expr-vars leaf)
                   (cond
                     ((member leaf key-exprs :test #'equal) nil)
                     ((and (symbolp leaf) (member leaf projected-names)) nil)
                     ((some (lambda (k) (and (consp k) (member leaf (%expr-subexprs k) :test #'equal)))
                            key-exprs)
                      (cypher-signal "AmbiguousAggregationExpression"
                                     :detail (ast-print (list :expr leaf))))
                     (t (cypher-signal "UndefinedVariable"
                                       :detail (ast-print (list :expr leaf))))))))
             (check-order-agg-calls (expr)
               ;; every aggregate call in the ORDER item must be a
               ;; projected aggregate (or a re-sort of one)
               (dolist (ac (%agg-calls-of expr))
                 (unless (some (lambda (p) (equal p ac)) projected-exprs)
                   (cypher-signal "UndefinedVariable"
                                  :detail (ast-print (list :expr ac))))))
             (check-rand (expr)
               (when (and (consp expr) (eq (car expr) :call)
                          (string-equal (getf (cdr expr) :fn) "rand"))
                 (cypher-signal "NonConstantExpression"
                                :detail "rand() is not allowed in aggregation"))
               (when (consp expr)
                 (dolist (c expr)
                   (when (consp c) (check-rand c))))))
      (dolist (item items)
        (when (expr-has-aggregate (getf (cdr item) :expr))
          (check-leaves (getf (cdr item) :expr))
          (check-rand (getf (cdr item) :expr))))
      (dolist (spec order)
        (let ((e (getf (cdr spec) :expr)))
          (cond
            ((and (expr-has-aggregate e) (not has-agg?))
             (cypher-signal "InvalidAggregation"
                            :detail "aggregation in ORDER BY"))
            ((expr-has-aggregate e)
             (check-order-agg-calls e)
             (check-order-leaves e))
            ((and has-agg? (not (expr-has-aggregate e)))
             ;; the whole expression may be a grouping key, and
             ;; variables used in the grouping keys may be referenced
             ;; (ORDER BY a.name + 'C' after WITH a.name AS name, ...)
             (unless (member e key-exprs :test #'equal)
               (let ((key-vars (loop for k in key-exprs append (%expr-vars k))))
                 (dolist (v (%expr-vars e))
                   (unless (or (member v projected-names)
                               (member v key-vars))
                     (cypher-signal "UndefinedVariable" :detail (symbol-name v)))))))))))))

(defun %check-set-items (items scope)
  (dolist (item items)
    (ecase (car item)
      ((:set-prop :add-prop)
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope)
       (%check-expr-types (getf (cdr item) :expr) scope))
      ((:set-map :add-map)
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope)
       (%check-expr-types (getf (cdr item) :expr) scope))
      (:set-var
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope)
       (%check-expr-types (getf (cdr item) :expr) scope))
      (:set-label (%check-var (getf (cdr item) :var) scope))
      (:set-labels (%check-var (getf (cdr item) :var) scope)))))

(defun %check-remove-items (items scope)
  (dolist (item items)
    (%check-var (getf (cdr item) :var) scope)))

(defun %check-clauses (clauses &optional (scope nil))
  "Validate CLAUSES against an initial variable SCOPE."
  (let ((scope scope))
    (dolist (clause clauses)
      (ecase (car clause)
        (:match
         (setf scope (%check-pattern (getf (cdr clause) :pattern) scope))
         (let ((where (getf (cdr clause) :where)))
           (when where
             (when (expr-has-aggregate where)
               (cypher-signal "InvalidAggregation"
                              :detail "aggregation is not allowed in WHERE"))
             (%check-expr-vars where scope))
           (when where (%check-expr-types where scope))))
        (:optional-match
         (setf scope (%check-pattern (getf (cdr clause) :pattern) scope))
         (let ((where (getf (cdr clause) :where)))
           (when where
             (when (expr-has-aggregate where)
               (cypher-signal "InvalidAggregation"
                              :detail "aggregation is not allowed in WHERE"))
             (%check-expr-vars where scope))
           (when where (%check-expr-types where scope))))
        (:with (setf scope (%check-projection clause scope)))
        (:return (setf scope (%check-projection clause scope)))
        (:unwind
         (let ((expr (getf (cdr clause) :expr))
               (var (getf (cdr clause) :var)))
           (when (expr-has-aggregate expr)
             (cypher-signal "InvalidAggregation" :detail "aggregation in UNWIND"))
           (%check-expr-vars expr scope)
           (%check-expr-types expr scope)
           ;; UNWIND rebinding shadows the previous binding (legal)
           (setf scope (acons var :other scope))))
        (:create
         (setf scope (%check-create-pattern (getf (cdr clause) :pattern) scope
                                           :require-directed t)))
        (:merge
         (let ((pre-scope scope))
           (setf scope (%check-create-pattern (getf (cdr clause) :pattern) scope))
           ;; ON MATCH / ON CREATE SET items are checked against the
           ;; scope at their point of application
           (dolist (items (append (getf (cdr clause) :on-create)
                                  (getf (cdr clause) :on-match)))
             (%check-set-items (if (and (consp items) (eq (car items) :set-items))
                                   (getf (cdr items) :set-items)
                                   (list items))
                               scope))))
        (:set (%check-set-items (getf (cdr clause) :items) scope))
        (:remove (%check-remove-items (getf (cdr clause) :items) scope))
        (:delete
         (dolist (expr (getf (cdr clause) :items))
           (%check-expr-vars expr scope)
           (%check-expr-types expr scope)
           (when (and (consp expr)
                      (member (car expr) '(:bin :un :lit :call)))
             (cypher-signal "InvalidArgumentType"
                            :detail "DELETE of a non-entity expression"))
          (when (and (consp expr)
                     (member (car expr) '(:has-labels)))
            (cypher-signal "InvalidDelete"
                           :detail "DELETE of a label/type expression"))))
        (:order
         (dolist (spec (getf (cdr clause) :items))
           (%check-expr-vars (getf (cdr spec) :expr) scope)
           (%check-expr-types (getf (cdr spec) :expr) scope)))
        (:skip (%check-constant (getf (cdr clause) :expr) scope))
        (:limit (%check-constant (getf (cdr clause) :expr) scope))
        (:union (cypher-signal "UnexpectedSyntax" :detail "UNION checked at query level"))))
    scope))

(defun cypher-check (ast)
  "Validate AST; signal the spec's errors.  Returns the AST."
  (%check-clauses (rest ast))
  ast)

(defun %return-columns (query)
  "Column names of a single query (for union checks)."
  (let ((ret (car (last (rest query)))))
    (if (eq (car ret) :return)
        (let ((items (getf (cdr ret) :items)))
          (if items
              (mapcar (lambda (item)
                        (or (getf (cdr item) :as)
                            (ast-var (ast-print
                                      (list :expr (getf (cdr item) :expr))))))
                      items)
              ;; RETURN *: width-only placeholders (the names depend on
              ;; the scope, resolved later)
              (loop for i below (length (rest query))
                    collect (ast-var (format nil "*column~d" i)))))
        (loop for i below (length (rest query))
              collect (ast-var (format nil "*column~d" i))))))

(defun %return-width (query)
  "Number of result columns of a single query (for union checks)."
  (length (%return-columns query)))

(defun cypher-check-query (ast)
  "Validate a (possibly union) query AST."
  (if (eq (car ast) :union)
      (let ((cols (mapcar #'%return-columns (getf (cdr ast) :queries))))
        (unless (every (lambda (c) (equal c (first cols))) cols)
          (cypher-signal "DifferentColumnsInUnion"
                         :detail (format nil "column names ~s differ" cols)))
        (dolist (q (getf (cdr ast) :queries))
          (cypher-check q))
        ast)
      (cypher-check ast)))
