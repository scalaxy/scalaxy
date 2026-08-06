;;;; node.lisp --- a single Scalaxy storage node

(in-package #:scalaxy)

(defstruct (node (:constructor %make-node (id store replicator followers started-at)))
  id
  store
  replicator
  followers   ; list of (follower-id . transport-fn)
  started-at) ; universal time when the node was created

(defvar *node-counter* 0)

(defun make-node (&key (id nil) (store (make-store)))
  (incf *node-counter*)
  (%make-node (or id (format nil "node-~d" *node-counter*))
              store
              (make-replicator)
              nil
              (get-universal-time)))

(defun node-next-seq (node)
  (let ((seq (replicator-seq (node-replicator node))))
    (setf (replicator-seq (node-replicator node)) (1+ seq))
    seq))

(defun node-add-follower (node follower-id transport)
  (push (cons follower-id transport) (node-followers node))
  node)

(defun node-replicate (node key value)
  "Forward a write to every configured follower.  Returns the number of
followers that acknowledged the write."
  (let ((acked 0)
        (seq (node-next-seq node)))
    (dolist (f (node-followers node))
      (let ((reply (ignore-errors
                    (funcall (cdr f)
                             (list :op #.+op-replicate+
                                   :seq seq
                                   :sub-op #.+op-put+
                                   :key key :value value)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (incf acked))))
    acked))

(defun node-put (node key value)
  (store-put (node-store node) key value)
  (node-replicate node key value)
  value)

(defun node-delete (node key)
  (let ((present? (store-delete (node-store node) key)))
    (when present?
      (node-next-seq node)
      (dolist (f (node-followers node))
        (ignore-errors
          (funcall (cdr f)
                   (list :op #.+op-replicate+
                         :seq (replicator-seq (node-replicator node))
                         :sub-op #.+op-delete+
                         :key key)))))
    present?))

(defun node-get (node key)
  (store-get (node-store node) key))

(defun node-scan (node prefix)
  (store-scan (node-store node) prefix))

(defun node-dispatch (node msg)
  "Handle a decoded request message and return a reply message plist.
Writes go through NODE-PUT/NODE-DELETE so they are replicated to the
node's followers (in-process or TCP) before the client is acknowledged."
  (case (getf msg :op)
    (#.+op-put+
     (node-put node (getf msg :key) (getf msg :value))
     (list :op #.+op-ack+ :status #.+status-ok+))
    (#.+op-get+
     (let ((value (store-get (node-store node) (getf msg :key))))
       (list :op #.+op-response+
             :status (if value #.+status-ok+ #.+status-not-found+)
             :value value)))
    (#.+op-delete+
     (node-delete node (getf msg :key))
     (list :op #.+op-ack+ :status #.+status-ok+))
    (#.+op-scan+
     (list :op #.+op-response+ :status #.+status-ok+
           :pairs (store-scan (node-store node) (getf msg :prefix))))
    (#.+op-replicate+
     (store-apply-log-record (node-store node) msg)
     (list :op #.+op-ack+ :seq (getf msg :seq) :status #.+status-ok+))
    (#.+op-snapshot+
     (list :op #.+op-snapshot+ :pairs (store-snapshot (node-store node))))
    (#.+op-ping+
     (list :op #.+op-pong+))
    (t (list :op #.+op-error+ :message (format nil "unknown opcode ~a"
                                               (getf msg :op))))))
