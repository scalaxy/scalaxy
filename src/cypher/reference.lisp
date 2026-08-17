;;;; reference.lisp --- the metacircular reference evaluator (oracle)
;;;;
;;;; Implements pattern matching literally per the specification
;;;; (plan B2): the match set of a pattern is the set of graph
;;;; homomorphisms, enumerated naively by trying every candidate for
;;;; every variable.  Exponential and slow by design; it exists to be
;;;; the executable specification against which the optimized
;;;; executor is differentially tested (law L13).

(in-package #:scalaxy)

(defun %ref-node-candidates (g node-el row graph params)
  "All nodes satisfying NODE-EL's constraints, evaluated in ROW."
  (let ((var (getf (cdr node-el) :var)))
    (if (and var (assoc var row))
        (list (row-get row var))
        (loop for eid in (graph-scan-node-ids g)
              for node = (graph-node g eid)
              when (%entity-matches node (getf (cdr node-el) :labels)
                                    (getf (cdr node-el) :props) row graph params)
                collect node))))

(defun %ref-rel-candidates (g rel-el node-el row graph params)
  "All (rel . node) pairs satisfying the (REL-EL NODE-EL) step."
  (let ((dir (getf (cdr rel-el) :dir))
        (rtype (getf (cdr rel-el) :type))
        (rprops (getf (cdr rel-el) :props))
        (rvar (getf (cdr rel-el) :var))
        (nvar (getf (cdr node-el) :var)))
    (let ((rels
            (if (and rvar (assoc rvar row))
                (list (row-get row rvar))
                (loop for rid in (graph-scan-rel-ids g :type rtype)
                      for rel = (graph-relationship g rid)
                      when (and rel (%entity-matches rel nil rprops row graph params))
                        collect rel))))
      (loop for rel in rels
            for eid = (getf rel :id)
            for start-node = (graph-node g (getf rel :start))
            for end-node = (graph-node g (getf rel :end))
            nconc
            (let ((pairs
                    (cond
                      ((eq dir :out)
                       (and start-node (list (cons rel end-node))))
                      ((eq dir :in)
                       (and end-node (list (cons rel start-node))))
                      (t (and start-node end-node
                              (list (cons rel end-node) (cons rel start-node)))))))
              (loop for (rel2 . node) in pairs
                    when (and node
                              (%entity-matches node (getf (cdr node-el) :labels)
                                               (getf (cdr node-el) :props) row graph params))
                      collect (cons rel2 node)))))))

(defun %ref-chain (g elements row graph params)
  "All rows extending ROW over ELEMENTS (hom-set enumeration)."
  (if (null elements)
      (list row)
      (let ((el (first elements)))
        (ecase (car el)
          (:node
           (let ((var (getf (cdr el) :var)))
             (loop for node in (%ref-node-candidates g el row graph params)
                   nconc (let ((row2 (%bind-entity-var row var node)))
                           (unless (eq row2 :fail)
                             (%ref-chain g (rest elements) row2 graph params)))))
          (:rel
           (let ((node-el (second elements))
                 (rvar (getf (cdr el) :var)))
             (loop for (rel . node) in (%ref-rel-candidates g el node-el row graph params)
                   nconc (let ((row2 (if rvar (row-bind row rvar rel) row)))
                           (let ((row3 (%bind-entity-var row2
                                                        (getf (cdr node-el) :var) node)))
                             (unless (eq row3 :fail)
                               (%ref-chain g (cddr elements) row3 graph params))))))))))))

(defun %ref-match-pattern (g rows pattern graph params)
  (let ((cur rows))
    (dolist (chain pattern)
      (unless (eq (car chain) :path-var)
        (setf cur (loop for row in cur
                        append (%ref-chain g chain row graph params)))))
    cur))

(defun reference-eval (query graph &key params)
  "Evaluate QUERY with the reference (hom-set) matcher: the executable
specification used as the differential-testing oracle."
  (cypher-query query graph :params params :matcher :reference))
