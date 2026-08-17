;;;; updates.lisp --- the effect algebra: CREATE/MERGE/SET/REMOVE/DELETE
;;;;
;;;; Updating clauses compile to effects applied through the graph-view
;;;; (axiom D4: every mutation goes through the KV store, so durability
;;;; and replication are inherited).  MERGE is idempotent (law L15):
;;;; match the whole pattern, create it only when nothing matches.

(in-package #:scalaxy)

(defun %set-one-prop (g entity k v)
  "Set property K of ENTITY to V; a NULL value removes the property."
  (let ((id (getf entity :id)))
    (if (cypher-null-p v)
        (if (%node-p entity)
            (graph-remove-node-property g id k)
            (graph-remove-relationship-property g id k))
        (if (%node-p entity)
            (graph-set-node-property g id k v)
            (graph-set-relationship-property g id k v)))))

(defun %apply-props (g entity map-v)
  "Replace ALL properties of ENTITY with the pairs of MAP-V (a map,
node or relationship); NULL values remove the property."
  (let ((pairs (if (cypher-map-p map-v)
                   (cypher-map-pairs map-v)
                   (getf map-v :props))))
    (let ((id (getf entity :id)))
      (dolist (k (mapcar #'car (getf entity :props)))
        (if (%node-p entity)
            (graph-remove-node-property g id k)
            (graph-remove-relationship-property g id k)))
      (dolist (p pairs)
        (%set-one-prop g entity (car p) (cdr p))))))

(defun %refresh-row-entities (g row)
  "Re-read node/relationship values bound in ROW (they may have been
modified by the clause)."
  (mapcar (lambda (p)
            (let ((v (cdr p)))
              (cond
                ((%node-p v) (cons (car p) (graph-node g (getf v :id))))
                ((%rel-p v) (cons (car p) (graph-relationship g (getf v :id))))
                (t p))))
          row))

(defun %row-entity (row var)
  "The node/relationship bound to VAR in ROW, or NIL."
  (let ((v (cdr (assoc var row))))
    (if (or (%node-p v) (%rel-p v)) v nil)))

(defun %filter-null-props (props)
  "CREATE ignores null property values (openCypher)."
  (remove-if (lambda (p) (%tv-null (cdr p))) props))

(defun %valid-prop-value-p (v)
  "Property values may be scalars or lists of scalars, not maps or
entities (openCypher InvalidPropertyType)."
  (cond
    ((%tv-null v) t)
    ((%node-p v) nil)
    ((%rel-p v) nil)
    ((%path-p v) nil)
    ((cypher-map-p v) nil)
    ((cypher-list-p v)
     (every (lambda (x)
              (and (not (cypher-map-p x)) (not (%node-p x)) (not (%rel-p x))))
            (cypher-list-elements v)))
    (t t)))

(defun %check-prop-values (props)
  "Signal InvalidPropertyType when a property map contains an invalid
value."
  (dolist (p props)
    (unless (%valid-prop-value-p (cdr p))
      (cypher-signal "InvalidPropertyType"
                     :detail (format nil "property ~a" (car p))))))

(defun %instantiate-node (g row node-el graph params)
  "Resolve or create the node of NODE-EL; returns (values row id)."
  (let* ((var (getf (cdr node-el) :var))
         (bound (and var (cdr (assoc var row))))
         (labels (getf (cdr node-el) :labels))
         (props (if (equal (getf (cdr node-el) :props) (list :empty-props t))
                    nil
                    (%filter-null-props
                     (mapcar (lambda (p)
                               (cons (car p) (eval-expr (cdr p) row graph params)))
                             (getf (cdr node-el) :props))))))
    (cond
      ((and bound (%node-p bound))
       (values row (getf bound :id)))
      ((and bound (not (%node-p bound)))
       (cypher-signal "VariableTypeConflict" :detail (symbol-name var)))
      (t
       (let ((eid (graph-create-node g :labels labels :props props)))
         (values (if var (row-bind row var (graph-node g eid)) row) eid))))))

(defun %create-clause (g rows clause graph params)
  (let ((out nil))
    (dolist (row rows)
      (let ((row2 row))
        (dolist (chain (getf (cdr clause) :pattern))
          (let ((elements (if (eq (car chain) :path-var) (cddr chain) chain)))
            (let ((ids nil))
              (loop for el in elements when (eq (car el) :node)
                    do (multiple-value-bind (r eid)
                           (%instantiate-node g row2 el graph params)
                         (push (cons el eid) ids)
                         (setf row2 r)))
              (loop for idx from 1 below (length elements) by 2
                    for rel = (nth idx elements)
                    for node-el = (nth (1+ idx) elements)
                    for src-el = (nth (1- idx) elements)
                    do (let ((src-id (cdr (assoc src-el ids)))
                             (end-id (cdr (assoc node-el ids)))
                             (r row2))
                         (let* ((props (%filter-null-props
                                        (mapcar (lambda (p)
                                                  (cons (car p)
                                                        (eval-expr (cdr p) r graph params)))
                                                (getf (cdr rel) :props))))
                                (start-id (if (eq (getf (cdr rel) :dir) :in)
                                              end-id src-id))
                                (finish-id (if (eq (getf (cdr rel) :dir) :in)
                                               src-id end-id))
                                (rid (graph-create-relationship
                                      g (getf (cdr rel) :type)
                                      start-id finish-id :props props))
                                (rvar (getf (cdr rel) :var)))
                           (setf row2 (if rvar
                                          (row-bind r rvar (graph-relationship g rid))
                                          r))))))))
        (push row2 out)))
    (nreverse out)))

(defun %merge-created-rel (g r els rel-el)
  "The created relationship for REL-EL in MERGE: the bound one when the
pattern names it, otherwise the relationship between the adjacent
created nodes."
  (let ((rv (getf (cdr rel-el) :var)))
    (if rv
        (let ((rel (graph-relationship g (getf (row-get r rv) :id))))
          (list* :start-node (graph-node g (getf rel :start))
                 :end-node (graph-node g (getf rel :end))
                 rel))
        (let* ((pos (position rel-el els))
               (src (row-get r (getf (cdr (nth (1- pos) els)) :var)))
               (dst (row-get r (getf (cdr (nth (1+ pos) els)) :var)))
               (rid (loop for (x . nb) in (graph-expand g (getf src :id) :dir :out)
                          when (equal nb (getf dst :id))
                            return x))
               (rel (graph-relationship g rid)))
          (list* :start-node (graph-node g (getf rel :start))
                 :end-node (graph-node g (getf rel :end))
                 rel)))))

(defun %merge-clause (g rows clause graph params)
  "MERGE: match the whole pattern anchored to bound vars; bind or create.
ON MATCH / ON CREATE SET items are applied to the matched/created rows."
  (let ((out nil))
    (dolist (row rows)
      (let ((matches (%match-clause g (list row)
                                    (list :match :pattern (getf (cdr clause) :pattern)
                                          :where nil)
                                    graph params)))
        (if matches
            ;; one output row per matched pattern (bound variables are
            ;; anchored, so MERGE matches every existing pattern instance)
            (dolist (r matches)
              (when (getf (cdr clause) :on-match)
                (setf r (first (%set-clause g (list r)
                                            (list :set :items (getf (cdr clause) :on-match))
                                            graph params))))
              (push r out))
            (let ((r (first (%create-clause g (list row) clause graph params))))
              (when (getf (cdr clause) :on-create)
                (setf r (first (%set-clause g (list r)
                                            (list :set :items (getf (cdr clause) :on-create))
                                            graph params))))
              ;; bind the path variable to the created path
              (let ((pat (getf (cdr clause) :pattern)))
                (when (and pat (eq (car (first pat)) :path-var))
                  (let* ((pv (second (first pat)))
                         (els (cddr (first pat)))
                         (nodes (loop for el in els
                                      when (eq (car el) :node)
                                        collect (row-get r (getf (cdr el) :var))))
                         (rels (loop for el in els
                                     when (eq (car el) :rel)
                                       collect (%merge-created-rel g r els el)))
                         (path (list :path
                                     (loop for n in nodes
                                           for i from 0
                                           append (if (zerop i)
                                                      (list n)
                                                      (list (nth (1- i) rels) n))))))
                    (setf r (row-bind r pv path)))))

              (push r out)))))
    (nreverse out)))

(defun %set-clause (g rows clause graph params)
  (dolist (item (getf (cdr clause) :items))
    (ecase (car item)
      (:set-prop
       (let ((var (getf (cdr item) :var))
             (prop (getf (cdr item) :prop)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (or (null entity) (%tv-null entity))
               ;; SET on a null entity (OPTIONAL MATCH miss) is a no-op
               (return))
             (let ((v (eval-expr (getf (cdr item) :expr) row graph params)))
               (%check-prop-values (list (cons prop v)))
               (cond
                 ((%node-p entity)
                  (if (%tv-null v)
                      (graph-remove-node-property g (getf entity :id) prop)
                      (graph-set-node-property g (getf entity :id) prop v)))
                 ((%rel-p entity)
                  (if (%tv-null v)
                      (graph-remove-relationship-property g (getf entity :id) prop)
                      (graph-set-relationship-property g (getf entity :id) prop v)))
                 (t (cypher-signal "UndefinedVariable" :detail (symbol-name var)))))))))
      (:add-prop
       (let ((var (getf (cdr item) :var))
             (prop (getf (cdr item) :prop)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (or (%node-p entity) (%rel-p entity))
               (let ((old (if (%node-p entity)
                              (graph-node-property g (getf entity :id) prop)
                              (graph-relationship-property g (getf entity :id) prop))))
                 (let ((v (eval-expr (getf (cdr item) :expr) row graph params)))
                   (let ((new (cond
                                ((and (cypher-map-p old) (cypher-map-p v))
                                 (cypher-map (append (cypher-map-pairs v)
                                                     (cypher-map-pairs old))))
                                ((and (cypher-map-p v) (cypher-null-p old)) v)
                                ((cypher-null-p old) (cypher-list (list v)))
                                ((cypher-list-p old)
                                 (cypher-list (append (cypher-list-elements old) (list v))))
                                (t (cypher-signal "InvalidArgumentValue"
                                                  :detail "+= requires a list or map property")))))
                     (cond
                       ((%node-p entity) (graph-set-node-property g (getf entity :id) prop new))
                       ((%rel-p entity) (graph-set-relationship-property g (getf entity :id) prop new)))))))))))
      (:set-map
       (let ((var (getf (cdr item) :var)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (or (%node-p entity) (%rel-p entity))
               (%apply-props g entity
                             (eval-expr (getf (cdr item) :expr) row graph params)))))))
      (:add-map
       (let ((var (getf (cdr item) :var)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (or (%node-p entity) (%rel-p entity))
               (let ((v (eval-expr (getf (cdr item) :expr) row graph params)))
                 (when (cypher-map-p v)
                   (dolist (p (cypher-map-pairs v))
                     (%set-one-prop g entity (car p) (cdr p))))))))))
      (:set-var
       (let ((var (getf (cdr item) :var)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (or (%node-p entity) (%rel-p entity))
               (let ((map-v (eval-expr (getf (cdr item) :expr) row graph params)))
                 (when (or (cypher-map-p map-v) (%node-p map-v) (%rel-p map-v))
                   (%apply-props g entity map-v))))))))
      (:set-label
       (let ((var (getf (cdr item) :var))
             (label (getf (cdr item) :label)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (%node-p entity)
               (graph-add-node-label g (getf entity :id) label))))))
      (:set-labels
       (let ((var (getf (cdr item) :var))
             (labels (getf (cdr item) :labels)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (%node-p entity)
               (dolist (label labels)
                 (graph-add-node-label g (getf entity :id) label)))))))))
  (mapcar (lambda (row) (%refresh-row-entities g row)) rows))

(defun %remove-clause (g rows clause graph params)
  (declare (ignore graph params))
  (dolist (item (getf (cdr clause) :items))
    (ecase (car item)
      (:remove-prop
       (let ((var (getf (cdr item) :var))
             (prop (getf (cdr item) :prop)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (cond
               ((%node-p entity) (graph-remove-node-property g (getf entity :id) prop))
               ((%rel-p entity) (graph-remove-relationship-property g (getf entity :id) prop)))))))
      (:remove-label
       (let ((var (getf (cdr item) :var))
             (label (getf (cdr item) :label)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (%node-p entity)
               (graph-remove-node-label g (getf entity :id) label))))))
      (:remove-labels
       (let ((var (getf (cdr item) :var))
             (labels (getf (cdr item) :labels)))
         (dolist (row rows)
           (let ((entity (%row-entity row var)))
             (when (%node-p entity)
               (dolist (label labels)
                 (graph-remove-node-label g (getf entity :id) label)))))))))
  ;; refresh the bound entities so subsequent clauses see the new state
  (mapcar (lambda (row) (%refresh-row-entities g row)) rows))

(defun %delete-clause (g rows clause graph params)
  (let ((detach? (getf (cdr clause) :detach)))
    (dolist (expr (getf (cdr clause) :items))
      (dolist (row rows)
        (let ((v (eval-expr expr row graph params)))
          (when (not (cypher-null-p v))
            (cond
              ((%node-p v) (graph-delete-node g (getf v :id) :detach detach?))
              ((%rel-p v) (graph-delete-relationship g (getf v :id)))
              ((%path-p v)
               ;; deleting a path deletes every element; relationships
               ;; first (DETACH is implied for paths)
               (let ((els (second v)))
                 (loop for i from 1 below (length els) by 2
                       do (graph-delete-relationship g (getf (nth i els) :id)))
                 (loop for i from 0 below (length els) by 2
                       do (graph-delete-node g (getf (nth i els) :id) :detach t))))
              ((cypher-list-p v)
               (dolist (x (cypher-list-elements v))
                 (cond
                   ((%node-p x) (graph-delete-node g (getf x :id) :detach detach?))
                   ((%rel-p x) (graph-delete-relationship g (getf x :id)))
                   ((%path-p x)
                    (let ((els (second x)))
                      (loop for i from 1 below (length els) by 2
                            do (graph-delete-relationship g (getf (nth i els) :id)))
                      (loop for i from 0 below (length els) by 2
                            do (graph-delete-node g (getf (nth i els) :id) :detach t)))))))
              (t (cypher-signal "InvalidArgumentValue"
                                :detail (format nil "DELETE of ~a" (cypher-type-name v))))))))))
  rows)
