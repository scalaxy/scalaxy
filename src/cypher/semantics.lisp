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

(defun %elem-kind (el)
  (ecase (car el) (:node :node) (:rel :rel)))

(defun %bind-pattern-var (scope var kind)
  (let ((existing (%in-scope var scope)))
    (cond
      ((null existing) (acons var kind scope))
      ((eq (cdr existing) kind) scope)
      ;; a variable with unknown type (null literal, parameter) may be
      ;; anchored as a node/rel/path and simply fails to match at runtime
      ((eq (cdr existing) :other) (acons var kind scope))
      ((eq kind :path)
       (cypher-signal "VariableAlreadyBound" :detail (symbol-name var)))
      (t (cypher-signal "VariableTypeConflict"
                        :detail (symbol-name var))))))

(defun %expr-vars (expr)
  "All variables referenced by EXPR."
  (cond
    ((symbolp expr) (list expr))
    ((atom expr) nil)
    ((eq (car expr) :var) (list (second expr)))
    ((member (car expr) '(:lit :param :count-*)) nil)
    ((eq (car expr) :prop) (%expr-vars (getf (cdr expr) :expr)))
    ((eq (car expr) :idx)
     (append (%expr-vars (getf (cdr expr) :expr))
             (%expr-vars (getf (cdr expr) :index))))
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
  "Check an EXISTS { chain [WHERE pred] } subquery: pattern variables
are subquery-local; the WHERE predicate may reference them."
  (let* ((chain (getf (cdr expr) :chain))
         (local (%pattern-chain-vars chain))
         (where (getf (cdr expr) :where)))
    (when where
      (dolist (v (%expr-vars where))
        (unless (member v local)
          (%check-var v scope))))))

(defun %check-expr-vars (expr scope)
  (cond
    ((and (consp expr) (eq (car expr) :exists))
     (%check-exists-pattern expr scope))
    ((and (consp expr) (eq (car expr) :exists-sub))
     (%check-exists-sub expr scope))
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
                    (cypher-signal "VariableAlreadyBound" :detail (symbol-name v))
                    (push v rel-vars))))))
        (dolist (el chain)
          (let ((var (getf (cdr el) :var)))
            (when var
              (when (and fresh (%in-scope var s))
                (cypher-signal "VariableAlreadyBound" :detail (symbol-name var)))
              (if (and in-path? (eq (cdr (%in-scope var s)) :path))
                  (cypher-signal "VariableAlreadyBound" :detail (symbol-name var))
                  (setf s (%bind-pattern-var s var (%elem-kind el)))))
            (dolist (p (getf (cdr el) :props))
              (let ((expr (cdr p)))
                (unless (atom expr)
                  (dolist (v (%expr-vars expr))
                    (unless (%in-scope v s)
                      (%check-var v scope))))))))))
    s))

(defun %check-create-pattern (pattern scope)
  "CREATE/MERGE pattern check.  A node variable bound before this clause
is legal only as a relationship anchor; a variable created within this
clause cannot be reused (VariableAlreadyBound), a standalone bound node
pattern would create a second entity (VariableAlreadyBound), and a
relationship variable that is already bound is an error."
  (let ((s scope)
        (created nil))
    (dolist (chain pattern)
      (let ((elements (if (eq (car chain) :path-var) (cddr chain) chain)))
        ;; relationship variables: always create fresh
        (dolist (el elements)
          (when (eq (car el) :rel)
            (when (getf (cdr el) :var-length)
              (cypher-signal "CreatingVarLength" :detail "variable-length relationships cannot be created"))
            (when (null (getf (cdr el) :type))
              (cypher-signal "NoSingleRelationshipType"
                             :detail "a relationship must have exactly one type"))
            (when (and (consp (getf (cdr el) :props))
                       (eq (car (getf (cdr el) :props)) :param))
              (cypher-signal "InvalidParameterUse"
                             :detail "parameter is not allowed as a relationship pattern"))
            (let ((v (getf (cdr el) :var)))
              (when v
                (when (or (%in-scope v s) (member v created))
                  (cypher-signal "VariableAlreadyBound" :detail (symbol-name v)))
                (push v created)))))
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
                (let ((expr (cdr pr)))
                  (unless (atom expr)
                    (dolist (v (%expr-vars expr))
                      (unless (or (%in-scope v s) (member v created))
                        (%check-var v scope)))))))))))
    ;; created entities are bound for subsequent clauses
    (dolist (v created)
      (setf s (acons v :node s)))
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
            (%check-agg-scope items order (car clause)))
          (dolist (item items)
            (let ((expr (getf (cdr item) :expr))
                  (as (getf (cdr item) :as)))
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
    (let ((order (getf (cdr clause) :order)))
      (when (and order
                 (not (some (lambda (s) (expr-has-aggregate (getf (cdr s) :expr)))
                            order))
                 (not (some (lambda (i) (expr-has-aggregate (getf (cdr i) :expr)))
                            items)))
        (dolist (spec order)
          (dolist (v (%expr-vars (getf (cdr spec) :expr)))
            (unless (or (%in-scope v scope)   ; input-scope variables
                        (%in-scope v new-scope))  ; projected aliases
              (cypher-signal "UndefinedVariable" :detail (symbol-name v))))))
    new-scope)))


(defun %agg-outer-subexprs (expr)
  "The maximal variable-bearing subexpressions of EXPR that contain no
aggregate calls (leaves of the aggregate-free skeleton)."
  (cond
    ((atom expr) nil)
    ((eq (car expr) :count-*) nil)
    ((and (eq (car expr) :call) (%aggregate-fn-p (getf (cdr expr) :fn))) nil)
    ((or (expr-has-aggregate expr)
         (member (car expr) '(:bin :neg :not :is-null :is-not-null :case)))
     (loop for c in (cdr expr)
           if (consp c) append (%agg-outer-subexprs c)
           else if (and c (symbolp c) (not (keywordp c))) collect c))
    (t (list expr))))

(defun %check-agg-scope (items order kind)
  "openCypher aggregation scope rules for projection items and their
ORDER BY subclause."
  (let ((key-exprs (loop for item in items
                         unless (expr-has-aggregate (getf (cdr item) :expr))
                           collect (getf (cdr item) :expr)))
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
                   (cond
                     ((null key-exprs)
                      (cypher-signal "UndefinedVariable"
                                     :detail (symbol-name (first (%expr-vars leaf)))))
                     ((not (member leaf key-exprs :test #'equal))
                      (cypher-signal "AmbiguousAggregationExpression"
                                     :detail (ast-print (list :expr leaf))))))))
             (check-rand (expr)
               (when (and (consp expr) (eq (car expr) :call)
                          (string-equal (getf (cdr expr) :fn) "rand"))
                 (cypher-signal "NonConstantExpression"
                                :detail "rand() is not allowed in aggregation"))
               (when (consp expr)
                 (dolist (c (cdr expr))
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
             (check-leaves e))
            ((and has-agg? (not (expr-has-aggregate e)))
             (dolist (v (%expr-vars e))
               (unless (member v projected-names)
                 (cypher-signal "UndefinedVariable" :detail (symbol-name v)))))))))))

(defun %check-set-items (items scope)
  (dolist (item items)
    (ecase (car item)
      ((:set-prop :add-prop)
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope))
      ((:set-map :add-map)
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope))
      (:set-var
       (%check-var (getf (cdr item) :var) scope)
       (%check-expr-vars (getf (cdr item) :expr) scope))
      (:set-label (%check-var (getf (cdr item) :var) scope)))))

(defun %check-remove-items (items scope)
  (dolist (item items)
    (%check-var (getf (cdr item) :var) scope)))

(defun cypher-check (ast)
  "Validate AST; signal the spec's errors.  Returns the AST."
  (let ((scope nil))
    (dolist (clause (rest ast))
      (ecase (car clause)
        (:match
         (setf scope (%check-pattern (getf (cdr clause) :pattern) scope))
         (let ((where (getf (cdr clause) :where)))
           (when where (%check-expr-vars where scope))))
        (:optional-match
         (setf scope (%check-pattern (getf (cdr clause) :pattern) scope))
         (let ((where (getf (cdr clause) :where)))
           (when where (%check-expr-vars where scope))))
        (:with (setf scope (%check-projection clause scope)))
        (:return (setf scope (%check-projection clause scope)))
        (:unwind
         (let ((expr (getf (cdr clause) :expr))
               (var (getf (cdr clause) :var)))
           (when (expr-has-aggregate expr)
             (cypher-signal "InvalidAggregation" :detail "aggregation in UNWIND"))
           (%check-expr-vars expr scope)
           ;; UNWIND rebinding shadows the previous binding (legal)
           (setf scope (acons var :other scope))))
        (:create
         (setf scope (%check-create-pattern (getf (cdr clause) :pattern) scope)))
        (:merge
         (setf scope (%check-create-pattern (getf (cdr clause) :pattern) scope)))
        (:set (%check-set-items (getf (cdr clause) :items) scope))
        (:remove (%check-remove-items (getf (cdr clause) :items) scope))
        (:delete
         (dolist (expr (getf (cdr clause) :items))
           (%check-expr-vars expr scope)))
        (:order
         (dolist (spec (getf (cdr clause) :items))
           (%check-expr-vars (getf (cdr spec) :expr) scope)))
        (:skip (%check-constant (getf (cdr clause) :expr) scope))
        (:limit (%check-constant (getf (cdr clause) :expr) scope))
        (:union (cypher-signal "UnexpectedSyntax" :detail "UNION checked at query level"))))
    ast))

(defun %return-width (query)
  "Number of result columns of a single query (for union checks)."
  (let ((ret (car (last (rest query)))))
    (if (eq (car ret) :return)
        (let ((items (getf (cdr ret) :items)))
          (if items (length items) (length (rest query))))  ; * approximated
        (length (rest query)))))

(defun cypher-check-query (ast)
  "Validate a (possibly union) query AST."
  (if (eq (car ast) :union)
      (let ((widths (mapcar #'%return-width (getf (cdr ast) :queries))))
        (unless (every (lambda (w) (= w (first widths))) widths)
          (cypher-signal "DifferentColumnsInUnion"
                         :detail (format nil "column counts ~s differ" widths)))
        (dolist (q (getf (cdr ast) :queries))
          (cypher-check q))
        ast)
      (cypher-check ast)))
