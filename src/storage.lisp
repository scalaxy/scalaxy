;;;; storage.lisp --- durable key/value storage
;;;;
;;;; In-memory hash table plus an append-only, length-prefixed mutation log
;;;; (the same record format as the network protocol).  On open, the log is
;;;; replayed to reconstruct state; on crash the tail may be truncated.

(in-package #:scalaxy)

(defstruct (store (:constructor %make-store (table log path &optional backend)))
  table
  log
  path
  ;; NIL/local or an S3-CONFIG.  The hash table remains a read cache and
  ;; startup loads the owned bucket before the store is returned.
  backend)

(defvar *s3-batch-queues* nil)

(defun %s3-batch-queue (cfg key op value)
  (let ((entry (assoc cfg *s3-batch-queues* :test #'eq)))
    (unless entry
        (setf entry (cons cfg (make-hash-table :test #'equal)))
      (push entry *s3-batch-queues*))
    ;; The key is already the hash-table key; do not retain a second copy
    ;; in the queue while a large import is resident in memory.
    (setf (gethash key (cdr entry)) (cons op value)))
  value)

(defun %s3-flush-batches ()
  (dolist (entry *s3-batch-queues*)
    (let ((cfg (car entry)) (queue (cdr entry)))
      (%s3-put-batch
       cfg (loop for key being the hash-keys of queue
                 using (hash-value record)
                 collect (list (car record) key (cdr record)))))))

(defvar *s3-batch-active* nil)

(defmacro with-s3-batch (() &body body)
  "Run bulk imports with one packed S3 object per backend.
Normal STORE-PUT remains synchronous; this explicit import mode flushes all
queued mutations before returning and is intended for rebuilds/imports."
  `(let ((*s3-batch-active* t)
         (*s3-batch-queues* nil))
     (unwind-protect (progn ,@body)
       (%s3-flush-batches))))

(defun make-store (&key path backend encryption-key s3-endpoint s3-bucket s3-access-key s3-secret-key
                         (s3-region "us-east-1") (s3-prefix "scalaxy/") cache-path lazy
                         owner-ring owner-id streaming-mode)
  "Create a store.  S3 configuration selects a write-through remote backend;
otherwise PATH selects the existing local append-only log."
  (let* ((backend (or backend
                       (when s3-endpoint
                         (make-s3-config :endpoint s3-endpoint :bucket s3-bucket
                                         :access-key s3-access-key :secret-key s3-secret-key
                                         :region s3-region :prefix s3-prefix
                                         :cache-dir cache-path :lazy lazy
                                         :owner-ring owner-ring :owner-id owner-id
                                         :streaming-mode streaming-mode))))
          ;; Encryption is applied transparently inside the S3 layer
          ;; (%s3-encrypt-body / %s3-decrypt-body via *s3-encryption-key*).
          ;; A separate wrapping plugin double-encrypted payloads and broke
          ;; every downstream typep check -- the root of KI-2.
          (encryption-key
           (when encryption-key
             (setf *s3-encryption-key*
                   (if (stringp encryption-key)
                       (string-to-octets encryption-key)
                       encryption-key))))
         (store (%make-store (make-hash-table :test #'equal) nil path backend)))
    (if backend
        (storage-plugin-load backend (store-table store))
        (when path
          (ensure-directories-exist path)
          (let ((stream (open path :direction :io
                                   :if-exists :append
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))))
            (setf (store-log store) stream)
            (store-replay store))))
    store))

(defun store-replay (store)
  "Replay the append-only log into the in-memory table."
  (let* ((path (store-path store))
         (size (with-open-file (in path :element-type '(unsigned-byte 8))
                 (file-length in))))
    (when (plusp size)
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((all (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence all in)
          (let ((pos 0))
            (loop while (< pos size)
                  do (multiple-value-bind (len j) (read-u32 all pos)
                       (store-apply-log-record store (decode-message (subseq all j (+ j len))))
                       (setf pos (+ j len))))))))))

(defun store-apply-log-record (store msg)
  "Apply a mutation message (PUT/DELETE or REPLICATE with :sub-op).
S3-backed stores persist replicated mutations directly; local stores only
update memory because the leader owns the durable log."
  (let ((op (or (getf msg :sub-op) (getf msg :op))))
    (ecase op
      (#.+op-put+
       (if (store-backend store)
           (if *s3-batch-queues*
               (%s3-batch-queue (store-backend store) (getf msg :key) "PUT" (getf msg :value))
               (storage-plugin-put (store-backend store) (getf msg :key) (getf msg :value))))
       (setf (gethash (getf msg :key) (store-table store)) (getf msg :value)))
      (#.+op-delete+
       (if (store-backend store)
           (if *s3-batch-queues*
               (%s3-batch-queue (store-backend store) (getf msg :key) "DELETE" nil)
               (storage-plugin-delete (store-backend store) (getf msg :key))))
       (remhash (getf msg :key) (store-table store))))
    msg))

(defun store-log-mutation (store msg)
  "Persist a mutation message to the append-only log."
  (let ((log (store-log store)))
    (when log
      (write-sequence (frame-message msg) log)
      (finish-output log))))

(defun store-put (store key value)
  ;; Remote persistence happens first: a failed S3 request must not expose a
  ;; value that was never durably uploaded.
  (if (store-backend store)
      (if *s3-batch-active*
          (%s3-batch-queue (store-backend store) key "PUT" value)
          (storage-plugin-put (store-backend store) key value))
      (store-log-mutation store (list :op #.+op-put+ :key key :value value)))
  (setf (gethash key (store-table store)) value)
  value)

(defun store-get (store key)
  (multiple-value-bind (value present) (gethash key (store-table store))
    (if present value
        (when (and (store-backend store)
                   (typep (store-backend store) 's3-config)
                   (s3-config-lazy (store-backend store)))
          (let ((v (storage-plugin-get (store-backend store) key)))
            (when v (setf (gethash key (store-table store)) v))
            v)))))

(defun store-delete (store key)
  (let ((present? (nth-value 1 (gethash key (store-table store)))))
    (when present?
      (if (store-backend store)
          (if *s3-batch-queues*
              (%s3-batch-queue (store-backend store) key "DELETE" nil)
              (storage-plugin-delete (store-backend store) key))
          (store-log-mutation store (list :op #.+op-delete+ :key key)))
      (remhash key (store-table store)))
    present?))

(defun %store-internal-key-p (key)
  (and (>= (length key) 2) (string= key "__" :end1 2)))

(defun store-map (store fn)
  "Visit stored pairs without allocating or sorting a result list."
  (maphash (lambda (k v)
             (unless (%store-internal-key-p k) (funcall fn k v)))
           (store-table store))
  (when (and (store-backend store)
             (typep (store-backend store) 's3-config)
             (s3-config-lazy (store-backend store)))
    (storage-plugin-map (store-backend store)
                        (lambda (k v)
                          (unless (%store-internal-key-p k)
                            (unless (nth-value 1 (gethash k (store-table store)))
                              (funcall fn k v))))))
  store)

(defun store-scan-all (store prefix)
  "Prefix scan that INCLUDES internal __ keys.  Used by subsystems
such as the durable replication outbox whose entries must survive
restarts but stay invisible to user scans."
  (let ((results nil))
    (maphash (lambda (k v)
               (when (and (>= (length k) (length prefix))
                          (string= prefix k :end2 (length prefix)))
                 (push (cons k v) results)))
             (store-table store))
    (sort results #'string< :key #'car)))

(defun store-scan-page (store prefix offset limit)
  "Return one bounded page of prefix matches without materializing the scan."
  (let ((seen 0) (out nil) (stop (+ offset limit)))
    (labels ((consider (key value)
               (when (and (not (%store-internal-key-p key))
                          (>= (length key) (length prefix))
                          (string= prefix key :end2 (length prefix)))
                 (when (and (>= seen offset) (< seen stop))
                   (push (cons key value) out))
                 (incf seen))))
      (maphash (lambda (k v) (consider k v)) (store-table store))
      (when (and (< (length out) limit) (store-backend store)
                 (typep (store-backend store) 's3-config)
                 (s3-config-lazy (store-backend store)))
        (maphash (lambda (k entry)
                   (declare (ignore entry))
                   (unless (nth-value 1 (gethash k (store-table store)))
                     (consider k (store-get store k))))
                 (s3-config-lazy-index (store-backend store)))))
    (nreverse out)))

(defun store-scan-fast (store prefix)
  "Return prefix matches without sorting; includes lazy S3 index values."
  (let ((results nil))
    (maphash (lambda (k v)
               (when (and (not (%store-internal-key-p k))
                          (>= (length k) (length prefix))
                          (string= prefix k :end2 (length prefix)))
                 (push (cons k v) results)))
             (store-table store))
    (when (and (store-backend store)
               (typep (store-backend store) 's3-config)
               (s3-config-lazy (store-backend store)))
      (maphash (lambda (k entry)
                 (declare (ignore entry))
                 (when (and (not (%store-internal-key-p k))
                            (>= (length k) (length prefix))
                            (string= prefix k :end2 (length prefix)))
                   (unless (nth-value 1 (gethash k (store-table store)))
                     (push (cons k (store-get store k)) results))))
               (s3-config-lazy-index (store-backend store))))
    results))

(defun store-scan (store prefix)
  "Return all (key . value) pairs whose key starts with PREFIX, sorted by key."
  (let ((results nil))
    (maphash (lambda (k v)
               (when (and (not (%store-internal-key-p k))
                          (>= (length k) (length prefix))
                          (string= prefix k :end2 (length prefix)))
                 (push (cons k v) results)))
             (store-table store))
    (sort results #'string< :key #'car)))

(defun store-count (store)
  (let ((n 0))
    (maphash (lambda (k v) (declare (ignore v))
               (unless (%store-internal-key-p k) (incf n)))
             (store-table store)) n))

(defun store-snapshot (store)
  "Return all (key . value) pairs as a fresh list."
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) (store-table store))
    pairs))

(defun store-restore (store pairs)
  "Replace the table contents with PAIRS (used by snapshot transfer)."
  (if (store-backend store)
      (progn
        (dolist (old (store-snapshot store))
          (unless (assoc (car old) pairs :test #'equal)
            (storage-plugin-delete (store-backend store) (car old))))
        (dolist (p pairs)
          (storage-plugin-put (store-backend store) (car p) (cdr p))))
      nil)
  (clrhash (store-table store))
  (dolist (p pairs)
    (setf (gethash (car p) (store-table store)) (cdr p)))
  store)
