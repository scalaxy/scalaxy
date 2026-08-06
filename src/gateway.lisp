;;;; gateway.lisp --- cluster gateway: routes key operations to ring owners
;;;;
;;;; A gateway keeps the cluster topology (node id -> data endpoint) and
;;;; routes each key operation over the TCP protocol to the node that owns
;;;; the key on the consistent-hash ring.  Scans fan out to every node and
;;;; merge/dedupe the results.  Status aggregation uses each node's HTTP
;;;; /api/node-status endpoint.

(in-package #:scalaxy)

(defstruct (gateway (:constructor %make-gateway (ring peers http-port)))
  ring       ; consistent-hash ring over node ids
  peers      ; alist: id -> (host data-port http-port-or-nil)
  http-port) ; default HTTP port when a peer has no explicit one

(defun make-gateway (&key (peers nil) (http-port 8080))
  "PEERS is a list of (id host data-port &optional http-port) tuples."
  (%make-gateway (make-ring :nodes (mapcar #'first peers))
                 (loop for (id host port hport) in peers
                       collect (cons id (list host port hport)))
                 http-port))

(defun gateway-peer-endpoint (gateway id)
  (cdr (assoc id (gateway-peers gateway) :test #'equal)))

(defun gateway-peer-host (gateway id)
  (first (gateway-peer-endpoint gateway id)))

(defun gateway-peer-port (gateway id)
  (second (gateway-peer-endpoint gateway id)))

(defun gateway-peer-http-port (gateway id)
  (or (third (gateway-peer-endpoint gateway id))
      (gateway-http-port gateway)))

(defun gateway-request (gateway id msg)
  (tcp-request (gateway-peer-host gateway id)
               (gateway-peer-port gateway id)
               msg))

(defun %ring-owner-order (gateway key)
  "Nodes that could hold KEY, owner first, then every other member.
Used for failover: replicas are stored on the other ring members, so
when the owner is unreachable the data can still be served."
  (let* ((owner (ring-lookup (gateway-ring gateway) key))
         (others (remove owner (mapcar #'car (gateway-peers gateway)) :test #'equal)))
    (cons owner others)))

(defun gateway-put (gateway key value)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-put+ :key key :value value)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-put reply)))))
  (error "Scalaxy: no reachable node for ~a" key))

(defun gateway-get (gateway key)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-get+ :key key)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-get (getf reply :value))))))
  nil)

(defun gateway-delete (gateway key)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-delete+ :key key)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-delete reply)))))
  nil)

(defun gateway-scan (gateway prefix &key (limit nil) (offset 0))
  "Scan every node, merge, dedupe and sort.  Returns a list of (key . value)."
  (let ((seen (make-hash-table :test #'equal))
        (pairs nil))
    (dolist (peer (gateway-peers gateway))
      (let* ((id (car peer))
             (reply (ignore-errors
                     (gateway-request gateway id (list :op #.+op-scan+ :prefix prefix)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (dolist (p (getf reply :pairs))
            (unless (gethash (car p) seen)
              (setf (gethash (car p) seen) t)
              (push p pairs))))))
    (let ((sorted (sort pairs #'string< :key #'car)))
      (if limit
          (subseq sorted offset (min (+ offset limit) (length sorted)))
          (subseq sorted offset)))))

(defun gateway-count (gateway)
  "Total number of distinct keys across the cluster."
  (length (gateway-scan gateway "")))

(defun gateway-status (gateway)
  "Aggregate per-node status over each node's HTTP endpoint.
Returns a plist with :nodes (list of status plists) and :total-keys."
  (let ((nodes nil)
        (total 0))
    (dolist (peer (gateway-peers gateway))
      (let* ((id (car peer))
             (host (gateway-peer-host gateway id))
             (reply (ignore-errors
                     (multiple-value-bind (status hdrs body)
                         (http-request host (gateway-peer-http-port gateway id)
                                       "GET" "/api/node-status")
                       (declare (ignore hdrs))
                       (when (and (= status 200) body)
                         (json-decode body))))))
        (if reply
            (progn
              (setf (gethash "id" reply) id)
              (push reply nodes)
              (incf total (or (gethash "keys" reply) 0)))
            (push (let ((h (make-hash-table :test #'equal)))
                    (setf (gethash "id" h) id
                          (gethash "status" h) "unreachable")
                    h)
                  nodes))))
    (list :nodes (nreverse nodes) :total-keys total)))
