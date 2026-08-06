;;;; storage.lisp --- durable key/value storage
;;;;
;;;; In-memory hash table plus an append-only, length-prefixed mutation log
;;;; (the same record format as the network protocol).  On open, the log is
;;;; replayed to reconstruct state; on crash the tail may be truncated.

(in-package #:scalaxy)

(defstruct (store (:constructor %make-store (table log path)))
  table
  log
  path)

(defun make-store (&key path)
  "Create a store.  If PATH is given, mutations are appended to an
append-only log at PATH and replayed on open."
  (let ((store (%make-store (make-hash-table :test #'equal) nil path)))
    (when path
      (ensure-directories-exist path)
      (let ((stream (open path :direction :io
                               :if-exists :append
                               :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))))
        (setf (store-log store) stream)
        (store-replay store)))
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
  "Apply a mutation message (PUT/DELETE or REPLICATE with :sub-op) to the
in-memory table without writing to the log."
  (let ((op (or (getf msg :sub-op) (getf msg :op))))
    (ecase op
      (#.+op-put+
       (setf (gethash (getf msg :key) (store-table store)) (getf msg :value)))
      (#.+op-delete+
       (remhash (getf msg :key) (store-table store))))
    msg))

(defun store-log-mutation (store msg)
  "Persist a mutation message to the append-only log."
  (let ((log (store-log store)))
    (when log
      (write-sequence (frame-message msg) log)
      (finish-output log))))

(defun store-put (store key value)
  (setf (gethash key (store-table store)) value)
  (store-log-mutation store (list :op #.+op-put+ :key key :value value))
  value)

(defun store-get (store key)
  (gethash key (store-table store)))

(defun store-delete (store key)
  (let ((present? (nth-value 1 (gethash key (store-table store)))))
    (when present?
      (remhash key (store-table store))
      (store-log-mutation store (list :op #.+op-delete+ :key key)))
    present?))

(defun store-scan (store prefix)
  "Return all (key . value) pairs whose key starts with PREFIX, sorted by key."
  (let ((results nil))
    (maphash (lambda (k v)
               (when (and (>= (length k) (length prefix))
                          (string= prefix k :end2 (length prefix)))
                 (push (cons k v) results)))
             (store-table store))
    (sort results #'string< :key #'car)))

(defun store-count (store)
  (hash-table-count (store-table store)))

(defun store-snapshot (store)
  "Return all (key . value) pairs as a fresh list."
  (let ((pairs nil))
    (maphash (lambda (k v) (push (cons k v) pairs)) (store-table store))
    pairs))

(defun store-restore (store pairs)
  "Replace the table contents with PAIRS (used by snapshot transfer)."
  (clrhash (store-table store))
  (dolist (p pairs)
    (setf (gethash (car p) (store-table store)) (cdr p)))
  store)
