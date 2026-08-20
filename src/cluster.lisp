;;;; cluster.lisp --- in-process cluster with consistent-hash routing
;;;; and synchronous leader replication.

(in-package #:scalaxy)

(defstruct (cluster (:constructor %make-cluster (ring nodes replicas)))
  ring
  nodes     ; hash-table node-id -> node
  replicas) ; number of synchronous replicas per write

(defun cluster-node-ids (cluster)
  (loop for k being the hash-keys of (cluster-nodes cluster) collect k))

(defun %wire-replication (cluster)
  "Point every node at the next REPLICAS nodes in ring order."
  (let ((ids (sort (cluster-node-ids cluster) #'string<))
        (nodes (cluster-nodes cluster)))
    (dolist (id ids)
      (setf (node-followers (gethash id nodes)) nil))
    (when (cdr ids)
      (dotimes (i (length ids))
        (let ((node (gethash (nth i ids) nodes)))
          (dotimes (r (cluster-replicas cluster))
            (let ((fid (nth (mod (+ i r 1) (length ids)) ids)))
              (unless (equal fid (nth i ids))
                (node-add-follower
                 node fid
                 (lambda (msg) (node-dispatch (gethash fid nodes) msg)))))))))))

(defun make-cluster (&key (ids nil) (vnodes-per-node 128) (replicas 1) (quorum 0))
  "Build a cluster from IDS (list of strings).  Writes go to the node that
owns the key on the consistent-hash ring; the owning node synchronously
replicates to the next REPLICAS nodes in ring order."
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (id ids)
      (setf (gethash id nodes) (make-node :id id :quorum quorum)))
    (let ((cluster (%make-cluster (make-ring :nodes ids
                                             :vnodes-per-node vnodes-per-node)
                                  nodes
                                  replicas)))
      (%wire-replication cluster)
      cluster)))

(defun cluster-add-node (cluster id)
  (setf (gethash id (cluster-nodes cluster)) (make-node :id id))
  (setf (cluster-ring cluster)
        (make-ring :nodes (cluster-node-ids cluster)
                   :vnodes-per-node (ring-vnodes-per-node (cluster-ring cluster))))
  (%wire-replication cluster)
  cluster)

(defun cluster-remove-node (cluster id)
  "Remove ID from the cluster, re-homing any keys it held to their new
ring owners so no data is lost (with replicas >= 1)."
  (let ((removed-store (node-store (gethash id (cluster-nodes cluster)))))
    (remhash id (cluster-nodes cluster))
    (setf (cluster-ring cluster)
          (make-ring :nodes (cluster-node-ids cluster)
                     :vnodes-per-node (ring-vnodes-per-node (cluster-ring cluster))))
    (%wire-replication cluster)
    ;; re-home keys that were stored on the removed node: for every key,
    ;; if its new ring owner does not already hold it, copy it there
    ;; (the owner's node-put re-replicates it onward).
    (maphash (lambda (k v)
               (let ((new-owner (ring-lookup (cluster-ring cluster) k)))
                 (when (and new-owner
                            (null (node-get (gethash new-owner (cluster-nodes cluster)) k)))
                   (node-put (gethash new-owner (cluster-nodes cluster)) k v))))
             (store-table removed-store))
    cluster))

(defun %primary (cluster key)
  (or (ring-lookup (cluster-ring cluster) key)
      (error "Scalaxy: empty cluster")))

(defun cluster-put (cluster key value)
  "Write KEY/VALUE to the owning node (with synchronous replication).
Returns the id of the primary node that accepted the write."
  (let ((id (%primary cluster key)))
    (node-put (gethash id (cluster-nodes cluster)) key value)
    id))

(defun cluster-get (cluster key)
  (let ((id (%primary cluster key)))
    (node-get (gethash id (cluster-nodes cluster)) key)))

(defun cluster-delete (cluster key)
  (let ((id (%primary cluster key)))
    (node-delete (gethash id (cluster-nodes cluster)) key)))

(defun cluster-scan (cluster prefix)
  "Scan across all nodes, de-duplicating keys that are held by replicas."
  (let ((seen (make-hash-table :test #'equal))
        (pairs nil))
    (maphash (lambda (id node)
               (declare (ignore id))
               (dolist (p (node-scan node prefix))
                 (unless (gethash (car p) seen)
                   (setf (gethash (car p) seen) t)
                   (push p pairs))))
             (cluster-nodes cluster))
    (sort pairs #'string< :key #'car)))

(defun cluster-create-database (cluster name)
  "Create database NAME in the in-process cluster."
  (unless (db-valid-name-p name)
    (error "Scalaxy: invalid database name ~s" name))
  (cluster-put cluster (db-key name "") #())
  name)

(defun cluster-list-databases (cluster)
  (mapcar #'car (db-list (cluster-scan cluster "d:"))))

(defun cluster-drop-database (cluster name)
  (when (equal name +default-db+)
    (error "Scalaxy: cannot drop the implicit database \"default\""))
  (dolist (p (cluster-scan cluster (db-key name "")))
    (cluster-delete cluster (car p)))
  (cluster-list-databases cluster))
