;;;; storage-plugins.lisp --- pluggable durable block stores and encryption
(in-package #:scalaxy)

(defgeneric storage-plugin-load (plugin table))
(defgeneric storage-plugin-get (plugin key))
(defgeneric storage-plugin-map (plugin fn))
(defgeneric storage-plugin-load-raw (plugin table))
(defgeneric storage-plugin-put-raw (plugin key bytes))
(defgeneric storage-plugin-put (plugin key value))
(defgeneric storage-plugin-delete (plugin key))
(defgeneric storage-plugin-scan (plugin prefix))

(defmethod storage-plugin-load ((plugin s3-config) table) (%s3-load plugin table))
(defmethod storage-plugin-get ((plugin s3-config) key)
  (if (s3-config-lazy plugin) (%s3-lazy-get plugin key) nil))
(defmethod storage-plugin-map ((plugin s3-config) fn)
  (maphash (lambda (key entry)
             (declare (ignore entry))
             (funcall fn key (storage-plugin-get plugin key)))
           (s3-config-lazy-index plugin)))
(defmethod storage-plugin-load-raw ((plugin s3-config) table)
  (%s3-load plugin table (lambda (bytes) bytes)))
(defmethod storage-plugin-put-raw ((plugin s3-config) key bytes) (%s3-put-raw plugin key bytes))
(defmethod storage-plugin-put ((plugin s3-config) key value) (%s3-put plugin key value))
(defmethod storage-plugin-delete ((plugin s3-config) key) (%s3-delete plugin key))
(defmethod storage-plugin-scan ((plugin s3-config) prefix)
  (let ((table (make-hash-table :test #'equal)))
    (%s3-load plugin table)
    (store-scan (%make-store table nil nil plugin) prefix)))

(defun %secure-random (n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (handler-case
        (with-open-file (s "/dev/urandom" :element-type '(unsigned-byte 8))
          (read-sequence v s))
      (error ()
        (replace v (%s3-sha256 (string-to-octets (format nil "~a~a" (get-universal-time) (random most-positive-fixnum)))))))
    v))

(defun %secure-xor (a b)
  (let ((out (make-array (length a) :element-type '(unsigned-byte 8))))
    (loop for i below (length a) do (setf (aref out i) (logxor (aref a i) (aref b i)))) out))

(defun %secure-encrypt (key plaintext)
  "Authenticated encryption using HMAC-SHA256 as a PRF keystream.
The format is SCX1 || nonce || ciphertext || authentication tag."
  (let* ((nonce (%secure-random 16))
         (cipher (make-array (length plaintext) :element-type '(unsigned-byte 8)))
         (pos 0) (counter 0))
    (loop while (< pos (length plaintext))
          for block = (%s3-hmac key (%s3-concat nonce
                                                 (let ((v (make-array 8 :element-type '(unsigned-byte 8))))
                                                   (loop for i below 8 do (setf (aref v (- 7 i)) (ldb (byte 8 (* i 8)) counter))) v)))
          do (loop for j below (min (length block) (- (length plaintext) pos))
                   do (setf (aref cipher (+ pos j)) (logxor (aref plaintext (+ pos j)) (aref block j))))
             (incf pos (min (length block) (- (length plaintext) pos))) (incf counter))
    (let* ((prefix (string-to-octets "SCX1"))
           (tag (%s3-hmac key (%s3-concat prefix nonce cipher))))
      (%s3-concat prefix nonce cipher tag))))

(defun %secure-decrypt (key payload)
  (unless (and (>= (length payload) 52)
               (equalp (subseq payload 0 4) (string-to-octets "SCX1")))
    (error "encrypted storage: invalid block header"))
  (let* ((prefix (subseq payload 0 4)) (nonce (subseq payload 4 20))
         (cipher (subseq payload 20 (- (length payload) 32)))
         (tag (subseq payload (- (length payload) 32)))
         (expected (%s3-hmac key (%s3-concat prefix nonce cipher))))
    (unless (equalp tag expected) (error "encrypted storage: authentication failed"))
    (%secure-encrypt/decrypt-stream key nonce cipher)))

(defun %secure-encrypt/decrypt-stream (key nonce cipher)
  (let ((plain (make-array (length cipher) :element-type '(unsigned-byte 8))) (pos 0) (counter 0))
    (loop while (< pos (length cipher))
          for block = (%s3-hmac key (%s3-concat nonce
                                                 (let ((v (make-array 8 :element-type '(unsigned-byte 8))))
                                                   (loop for i below 8 do (setf (aref v (- 7 i)) (ldb (byte 8 (* i 8)) counter))) v)))
          do (loop for j below (min (length block) (- (length cipher) pos))
                   do (setf (aref plain (+ pos j)) (logxor (aref cipher (+ pos j)) (aref block j))))
             (incf pos (min (length block) (- (length cipher) pos))) (incf counter))
    plain))

(defgeneric storage-plugin-lazy-index (plugin))

(defmethod storage-plugin-lazy-index ((plugin s3-config))
  (s3-config-lazy-index plugin))

(defmethod storage-plugin-lazy-index ((plugin t))
  nil)

(defun %s3-get-raw-object (cfg key)
  "Direct GET of the object stored under KEY (hex-encoded), binary body
or NIL when absent.  Bypasses the lazy index entirely so encrypted
wrappers can fetch exactly what they put."
  (multiple-value-bind (status headers body)
      (%s3-call cfg "GET" (%s3-hex-key key) :binary t)
    (declare (ignore headers))
    (when (= status 200) body)))

(defstruct (encrypted-storage-plugin (:constructor %make-encrypted-storage-plugin (inner key))) inner key)

;; The wrapper is transparent: values crossing the boundary are codec
;; octets exactly like the plain store produces and consumes.
(defmethod storage-plugin-lazy-index ((plugin encrypted-storage-plugin))
  (storage-plugin-lazy-index (encrypted-storage-plugin-inner plugin)))

(defmethod storage-plugin-get ((plugin encrypted-storage-plugin) key)
  (let* ((key (encrypted-storage-plugin-key plugin))
         (bytes (%s3-get-raw-object (encrypted-storage-plugin-inner plugin) key)))
    (when bytes
      ;; decrypt -> one codec decode: yields the same encoded octets a
      ;; plaintext store would hand back.
      (car (multiple-value-list (codec-decode (%secure-decrypt key bytes)))))))

(defmethod storage-plugin-map ((plugin encrypted-storage-plugin) fn)
  (let* ((inner (encrypted-storage-plugin-inner plugin))
         (enc-key (encrypted-storage-plugin-key plugin))
         (index (storage-plugin-lazy-index inner)))
    (when index
      (maphash
       (lambda (k entry)
         (declare (ignore entry))
         (let ((bytes (%s3-get-raw-object inner k)))
           (when bytes
             (funcall fn k
                      (car (multiple-value-list
                            (codec-decode (%secure-decrypt enc-key bytes))))))))
       index))))
(defmethod storage-plugin-load ((plugin encrypted-storage-plugin) table)
  (let ((inner (encrypted-storage-plugin-inner plugin)) (key (encrypted-storage-plugin-key plugin)))
    (let ((raw (make-hash-table :test #'equal)))
      (storage-plugin-load-raw inner raw)
      (maphash (lambda (k bytes)
                 (setf (gethash k table)
                       (car (multiple-value-list (codec-decode (%secure-decrypt key bytes)))))) raw))))
(defmethod storage-plugin-put ((plugin encrypted-storage-plugin) k v)
  (let ((bytes (%secure-encrypt (encrypted-storage-plugin-key plugin) (codec-encode v))))
    (storage-plugin-put-raw (encrypted-storage-plugin-inner plugin) k bytes)))
(defmethod storage-plugin-delete ((plugin encrypted-storage-plugin) k)
  (storage-plugin-delete (encrypted-storage-plugin-inner plugin) k))

(defun make-encrypted-storage-plugin (plugin key)
  (unless (and key (plusp (length key))) (error "encryption key must be non-empty octets"))
  (%make-encrypted-storage-plugin plugin key))


(defstruct (graph-db-backend (:constructor %make-graph-db-backend (host port db prefix)))
  host port db prefix)

(defun %hex-octets (hex)
  (let ((out (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (loop for i below (length out)
          do (setf (aref out i) (parse-integer hex :start (* i 2) :end (+ (* i 2) 2) :radix 16))) out))
(defun %graph-db-request (backend query params)
  (let ((body (make-hash-table :test #'equal)))
    (setf (gethash "query" body) query (gethash "params" body) params
          (gethash "db" body) (graph-db-backend-db backend))
    (multiple-value-bind (status headers response)
        (http-request (graph-db-backend-host backend) (graph-db-backend-port backend)
                      "POST" "/api/cypher"
                      :headers (list (cons "Content-Type" "application/json"))
                      :body (json-encode body))
      (declare (ignore headers))
      (unless (= status 200) (error "graph storage backend HTTP ~d: ~a" status response))
      (json-decode response))))

(defun make-graph-db-backend (endpoint &key (db "default") (prefix "scalaxy/"))
  (let* ((p (search "://" endpoint))
         (rest (if p (subseq endpoint (+ p 3)) endpoint))
         (slash (position #\/ rest))
         (authority (subseq rest 0 (or slash (length rest))))
         (colon (position #\: authority)))
    (%make-graph-db-backend (if colon (subseq authority 0 colon) authority)
                             (if colon (parse-integer (subseq authority (1+ colon))) 8080)
                             db prefix)))
(defun %graph-row-pairs (response)
  (let ((rows (if (listp response) response (gethash "rows" response))))
    (loop for row in rows
          for k = (or (and (hash-table-p row) (gethash "k" row)) (and (consp row) (cdr (assoc "k" row :test #'equal))))
          for v = (or (and (hash-table-p row) (gethash "v" row)) (and (consp row) (cdr (assoc "v" row :test #'equal))))
          when (and k v) collect (cons k v))))
(defmethod storage-plugin-load-raw ((plugin graph-db-backend) table)
  (dolist (p (%graph-row-pairs (%graph-db-request plugin
                                                    "MATCH (b:ScalaxyBlock) RETURN b.k AS k, b.v AS v"
                                                    (make-hash-table :test #'equal))))
    (setf (gethash (subseq (car p) (length (graph-db-backend-prefix plugin))) table)
          (%hex-octets (cdr p)))))
(defmethod storage-plugin-put-raw ((plugin graph-db-backend) key bytes)
  (let ((params (make-hash-table :test #'equal)))
    (setf (gethash "k" params) (concatenate 'string (graph-db-backend-prefix plugin) key)
          (gethash "v" params) (hex-digest bytes))
    (%graph-db-request plugin "MERGE (b:ScalaxyBlock {k: $k}) SET b.v = $v" params) bytes))
(defmethod storage-plugin-load ((plugin graph-db-backend) table)
  (let ((pairs (%graph-row-pairs (%graph-db-request plugin
                                                    "MATCH (b:ScalaxyBlock) RETURN b.k AS k, b.v AS v"
                                                    (make-hash-table :test #'equal)))))
    (dolist (p pairs) (setf (gethash (subseq (car p) (length (graph-db-backend-prefix plugin))) table)
                             (car (multiple-value-list (codec-decode (%hex-octets (cdr p)))))))))
(defmethod storage-plugin-put ((plugin graph-db-backend) key value)
  (let ((params (make-hash-table :test #'equal)))
    (setf (gethash "k" params) (concatenate 'string (graph-db-backend-prefix plugin) key)
          (gethash "v" params) (hex-digest (codec-encode value)))
    (%graph-db-request plugin "MERGE (b:ScalaxyBlock {k: $k}) SET b.v = $v" params) value))
(defmethod storage-plugin-delete ((plugin graph-db-backend) key)
  (let ((params (make-hash-table :test #'equal)))
    (setf (gethash "k" params) (concatenate 'string (graph-db-backend-prefix plugin) key))
    (%graph-db-request plugin "MATCH (b:ScalaxyBlock {k: $k}) DELETE b" params) t))
(defmethod storage-plugin-scan ((plugin graph-db-backend) prefix)
  (let ((table (make-hash-table :test #'equal)))
    (storage-plugin-load plugin table)
    (let ((out nil)) (maphash (lambda (k v) (when (and (>= (length k) (length prefix)) (string= prefix k :end2 (length prefix))) (push (cons k v) out))) table) (sort out #'string< :key #'car))))
