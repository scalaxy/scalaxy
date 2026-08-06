;;;; consistent-hash.lisp --- consistent hashing ring
;;;;
;;;; Classic consistent hashing with virtual nodes: each physical node is
;;;; hashed VNODES-PER-NODE times onto a sorted ring of hash values.  Adding
;;;; or removing a node moves only the keys that previously belonged to that
;;;; node (about 1/N of the keyspace for a uniform distribution).

(in-package #:scalaxy)

(defstruct (ring (:constructor %make-ring (vnodes nodes vnodes-per-node)))
  vnodes          ; sorted vector of (hash . node-id)
  nodes           ; list of node ids
  vnodes-per-node)

(defun %rebuild (node-ids vnodes-per-node)
  (let ((entries (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (node node-ids)
      (dotimes (i vnodes-per-node)
        (vector-push-extend (cons (hash-string (format nil "~a#~d" node i)) node)
                            entries)))
    (sort entries #'< :key #'car)))

(defun make-ring (&key (nodes nil) (vnodes-per-node 128))
  (%make-ring (%rebuild nodes vnodes-per-node) (copy-list nodes) vnodes-per-node))

(defun ring-add (ring node)
  (let ((ids (cons node (ring-nodes ring))))
    (%make-ring (%rebuild ids (ring-vnodes-per-node ring))
                ids
                (ring-vnodes-per-node ring))))

(defun ring-remove (ring node)
  (let ((ids (remove node (ring-nodes ring) :test #'equal)))
    (%make-ring (%rebuild ids (ring-vnodes-per-node ring))
                ids
                (ring-vnodes-per-node ring))))

(defun %lower-bound (vec h)
  "Index of the first entry with hash >= H (vec is sorted by hash)."
  (let ((lo 0) (hi (length vec)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (< (car (aref vec mid)) h)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    lo))

(defun ring-lookup (ring key)
  "Return the node id responsible for KEY."
  (let* ((h (hash-string key))
         (vnodes (ring-vnodes ring)))
    (if (zerop (length vnodes))
        nil
        (let ((i (%lower-bound vnodes h)))
          (cdr (aref vnodes (if (= i (length vnodes)) 0 i)))))))
