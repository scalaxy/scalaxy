;;;; s3.lisp --- S3-compatible object storage backend
;;;;
;;;; Minimal AWS Signature V4 client for Garage and other S3-compatible services.

(in-package #:scalaxy)

(defparameter +s3-sha256-k+
  #( #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
     #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
     #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
     #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
     #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
     #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
     #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
     #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
     #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %s3-u32 (x) (logand x #xffffffff))
(defun %s3-rotr (x n)
  (logand (logior (ash x (- n)) (ash x (- 32 n))) #xffffffff))
(defun %s3-sha256 (data)
  (let* ((n (length data)) (bits (* n 8))
         (total (* 64 (ceiling (+ n 9) 64)))
         (buf (make-array total :element-type '(unsigned-byte 8) :initial-element 0))
         (h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
    (replace buf data)
    (setf (aref buf n) #x80)
    (loop for i from 0 below 8
          do (setf (aref buf (- total 1 i)) (ldb (byte 8 (* i 8)) bits)))
    (loop for base from 0 below total by 64
          do (let ((w (make-array 64 :element-type '(unsigned-byte 32))))
               (loop for i below 16
                     do (setf (aref w i)
                              (%s3-u32 (logior (ash (aref buf (+ base (* i 4))) 24)
                                              (ash (aref buf (+ base (* i 4) 1)) 16)
                                              (ash (aref buf (+ base (* i 4) 2)) 8)
                                              (aref buf (+ base (* i 4) 3))))))
               (loop for i from 16 below 64
                     for a = (aref w (- i 15)) for b = (aref w (- i 2))
                     do (setf (aref w i)
                              (%s3-u32 (+ (logxor (%s3-rotr a 7) (%s3-rotr a 18) (ash a -3))
                                          (logxor (%s3-rotr b 17) (%s3-rotr b 19) (ash b -10))
                                          (aref w (- i 16)) (aref w (- i 7))))))
               (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                     (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
                 (loop for i below 64
                       for s1 = (logxor (%s3-rotr e 6) (%s3-rotr e 11) (%s3-rotr e 25))
                       for ch = (logxor (logand e f) (logand (lognot e) g))
                       for t1 = (%s3-u32 (+ hh s1 ch (aref +s3-sha256-k+ i) (aref w i)))
                       for s0 = (logxor (%s3-rotr a 2) (%s3-rotr a 13) (%s3-rotr a 22))
                       for maj = (logxor (logand a b) (logand a c) (logand b c))
                       for t2 = (%s3-u32 (+ s0 maj))
                       do (setf hh g g f f e e (%s3-u32 (+ d t1)) d c c b
                                b a a (%s3-u32 (+ t1 t2))))
                 (loop for i below 8
                       do (setf (aref h i) (%s3-u32 (+ (aref h i)
                                                        (case i (0 a) (1 b) (2 c) (3 d)
                                                              (4 e) (5 f) (6 g) (7 hh)))))))))
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i below 8
            for x = (aref h i)
            do (loop for j below 4
                     do (setf (aref out (+ (* i 4) j)) (ldb (byte 8 (* (- 3 j) 8)) x))))
      out)))

(defun %s3-hex (bytes) (string-downcase (hex-digest bytes)))

(defun %s3-xor-octets (a byte)
  (let ((out (make-array (length a) :element-type '(unsigned-byte 8))))
    (loop for i below (length a) do (setf (aref out i) (logxor (aref a i) byte))) out))
(defun %s3-concat (&rest vectors)
  (let ((out (make-array (reduce #'+ vectors :key #'length) :element-type '(unsigned-byte 8))) (p 0))
    (dolist (v vectors) (replace out v :start1 p) (incf p (length v))) out))
(defun %s3-hmac (key data)
  (let* ((k (if (> (length key) 64) (%s3-sha256 key) key))
         (kp (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace kp k)
    (%s3-sha256 (%s3-concat (%s3-xor-octets kp #x5c)
                            (%s3-sha256 (%s3-concat (%s3-xor-octets kp #x36) data))))))
(defun %s3-hmac-string (key string) (%s3-hmac key (string-to-octets string)))

(defstruct (s3-config (:constructor %make-s3-config (endpoint host port bucket access-key secret-key region prefix cache-dir cache-max-bytes lazy lazy-index lazy-segments lazy-counts lazy-labels lazy-label-ids lazy-endpoint-aggregates lazy-type-counts lazy-sums summary-valid cache-hits cache-misses cache-bytes lazy-aggregate-cache lazy-topk-summaries owner-ring owner-id encryption-key streaming-mode)))
  endpoint host port bucket access-key secret-key region prefix cache-dir cache-max-bytes lazy lazy-index lazy-segments lazy-counts lazy-labels lazy-label-ids lazy-endpoint-aggregates lazy-type-counts lazy-sums summary-valid cache-hits cache-misses cache-bytes lazy-aggregate-cache lazy-topk-summaries owner-ring owner-id encryption-key streaming-mode)

(defun make-s3-config (&key endpoint bucket access-key secret-key (region "us-east-1")
                              (prefix "scalaxy/") cache-dir lazy owner-ring owner-id encryption-key streaming-mode
                              (cache-max-bytes (let ((v (uiop:getenv "SCALAXY_S3_CACHE_MAX_BYTES")))
                                                 (and v (ignore-errors (parse-integer v))))))
  "Create an S3 configuration.  HTTP endpoints are supported for local Garage testing."
  (unless (and endpoint bucket access-key secret-key) (error "S3 requires endpoint, bucket, access-key and secret-key"))
  (let* ((scheme-pos (search "://" endpoint))
         (scheme (if scheme-pos (subseq endpoint 0 scheme-pos) "http"))
         (rest (if scheme-pos (subseq endpoint (+ scheme-pos 3)) endpoint))
         (slash (position #\/ rest))
         (authority (subseq rest 0 (or slash (length rest))))
         (colon (position #\: authority))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon (parse-integer (subseq authority (1+ colon)))
                   (if (string-equal scheme "https") 443 80))))
    (unless (string-equal scheme "http") (error "S3 endpoint must use http:// for this build (Garage local mode)"))
    (%make-s3-config endpoint host port bucket access-key secret-key region
                     (if (and prefix (plusp (length prefix)))
                         (format nil "~a/" (string-right-trim "/" prefix))
                         "")
                     (and cache-dir (uiop:ensure-directory-pathname cache-dir))
                     (or cache-max-bytes 0)
                     lazy (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     t 0 0 0
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     owner-ring owner-id encryption-key streaming-mode)))

(defun %s3-meta-owned-p (cfg key)
  "True when KEY belongs to this node per the cluster ring."
  (let ((ring (s3-config-owner-ring cfg)))
    (or (null ring)
        (equal (ring-lookup ring key) (s3-config-owner-id cfg)))))

(defun %s3-hex-key (key)
  ;; Encode Unicode code points directly so object names remain reversible
  ;; without depending on the process locale or a UTF-8 library.
  (with-output-to-string (out)
    (loop for ch across key do (format out "~8,'0X" (char-code ch)))))
(defun %s3-unhex-key (hex)
  (let ((out (make-string (floor (length hex) 8))))
    (loop for i below (length out)
          do (setf (aref out i)
                   (code-char (parse-integer hex :start (* i 8) :end (+ (* i 8) 8) :radix 16))))
    out))
(defun %s3-xml-values (xml tag)
  (let ((open (concatenate 'string "<" tag ">"))
        (close (concatenate 'string "</" tag ">"))
        (cursor 0) (values nil))
    (loop for start = (search open xml :start2 cursor)
          while start
          for value-start = (+ start (length open))
          for end = (search close xml :start2 value-start)
          while end
          do (push (subseq xml value-start end) values)
             (setf cursor (+ end (length close))))
    (nreverse values)))

(defun %s3-url-encode (string)
  (with-output-to-string (out)
    (loop for b across (string-to-octets string)
          do (if (or (<= (char-code #\A) b (char-code #\Z))
                     (<= (char-code #\a) b (char-code #\z))
                     (<= (char-code #\0) b (char-code #\9))
                     (member b '(45 46 95 126)))
                 (write-char (code-char b) out)
                 (progn (write-char #\% out) (format out "~2,'0X" b))))))

(defvar *s3-encryption-key* nil
  "When non-nil, all object data written to S3 is encrypted with
authenticated HMAC-SHA256 encryption before upload and decrypted after
download.  Set from SCALAXY_S3_ENCRYPTION_KEY.")

(defun %s3-encrypt-body (bytes)
  "Encrypt BYTES for S3 storage when encryption is enabled."
  (if *s3-encryption-key* (%secure-encrypt *s3-encryption-key* bytes) bytes))

(defun %s3-decrypt-body (bytes)
  "Decrypt BYTES fetched from S3 when encryption is enabled."
  (if (and *s3-encryption-key*
           (> (length bytes) 4)
           (equalp (subseq bytes 0 4) (string-to-octets "SCX1")))
      (%secure-decrypt *s3-encryption-key* bytes)
      bytes))

(defun %s3-date ()
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time) 0)
    (values (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour min sec)
            (format nil "~4,'0d~2,'0d~2,'0d" year month day))))
(defun %s3-path (cfg object)
  (format nil "/~a/~a" (s3-config-bucket cfg)
          (if (plusp (length object))
              (concatenate 'string (s3-config-prefix cfg) object)
              "")))
(defun %s3-call (cfg method object &key (query "") (body (make-array 0 :element-type '(unsigned-byte 8))) (binary nil))
  (multiple-value-bind (amz-date short-date) (%s3-date)

    (let* ((payload-hash (%s3-hex (%s3-sha256 body)))
           (path (%s3-path cfg object))
           (host (format nil "~a:~d" (s3-config-host cfg) (s3-config-port cfg)))
           (canonical-headers (format nil "host:~a~%x-amz-content-sha256:~a~%x-amz-date:~a~%" host payload-hash amz-date))
           (signed "host;x-amz-content-sha256;x-amz-date")
           (canonical (format nil "~a~%~a~%~a~%~a~%~a~%~a" method path query canonical-headers signed payload-hash))
           (scope (format nil "~a/~a/s3/aws4_request" short-date (s3-config-region cfg)))
           (string-to-sign (format nil "AWS4-HMAC-SHA256~%~a~%~a~%~a" amz-date scope (%s3-hex (%s3-sha256 (string-to-octets canonical)))))
           (kdate (%s3-hmac-string (string-to-octets (concatenate 'string "AWS4" (s3-config-secret-key cfg))) short-date))
           (kregion (%s3-hmac-string kdate (s3-config-region cfg)))
           (kservice (%s3-hmac-string kregion "s3"))
           (ksigning (%s3-hmac-string kservice "aws4_request"))
           (signature (%s3-hex (%s3-hmac-string ksigning string-to-sign)))
           (auth (format nil "AWS4-HMAC-SHA256 Credential=~a/~a, SignedHeaders=~a, Signature=~a"
                         (s3-config-access-key cfg) scope signed signature)))
      (http-request (s3-config-host cfg) (s3-config-port cfg) method
                    (if (plusp (length query)) (format nil "~a?~a" path query) path)
                    :headers (list (cons "X-Amz-Date" amz-date)
                                   (cons "X-Amz-Content-Sha256" payload-hash) (cons "Authorization" auth))
                    :body body :binary binary))))

(defun %s3-apply-batch-bytes (bytes table)
  "Apply a packed mutation segment without materializing its full list.
The segment is a codec list of MAGIC and a second codec list of records."
  (unless (= (aref bytes 0) +tag-list+) (return-from %s3-apply-batch-bytes nil))
  (multiple-value-bind (outer-count pos) (read-u32 bytes 1)
    (declare (ignore outer-count))
    (multiple-value-bind (magic next) (%codec-read bytes pos)
      (unless (string= magic "scalaxy-s3-batch-v1") (return-from %s3-apply-batch-bytes nil))
      (unless (= (aref bytes next) +tag-list+) (return-from %s3-apply-batch-bytes nil))
      (multiple-value-bind (count cursor) (read-u32 bytes (1+ next))
        (loop repeat count
              do (multiple-value-bind (record after) (%codec-read bytes cursor)
                   (let ((op (first record)) (key (second record)))
                     (if (string= op "PUT")
                         (setf (gethash key table) (third record))
                         (remhash key table)))
                   (setf cursor after)))))))

(defun %s3-cache-file (cfg relative)
  (let ((dir (s3-config-cache-dir cfg)))
    (when dir
      (merge-pathnames (format nil "~a.bin" (%s3-hex (%s3-sha256 (string-to-octets relative)))) dir))))

(defun %s3-meta-path (cfg name)
  (let ((dir (s3-config-cache-dir cfg)))
    (when dir (merge-pathnames name dir))))

(defun %s3-write-sexp (path value)
  (when path
    (ensure-directories-exist path)
    (let ((tmp (format nil "~a.tmp.~d.~d" path (get-universal-time)
                       (random 1000000000))))
      (with-open-file (out tmp :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (with-standard-io-syntax (prin1 value out) (terpri out)))
      (rename-file tmp path))))

(defun %s3-read-sexp (path)
  (when (and path (probe-file path))
    (handler-case
        (with-open-file (in path :external-format :utf-8)
          (with-standard-io-syntax (read in nil nil)))
      (error () nil))))

(defun %s3-index-sidecar-path (cfg relative)
  (%s3-meta-path cfg
                 (format nil "lazy-index-~a.idx"
                         (%s3-hex (%s3-sha256 (string-to-octets relative))))))

(defun %s3-bloom-add (bloom key)
  (let ((digest (%s3-sha256 (string-to-octets key))))
    (dotimes (i 4)
      (let* ((base (* i 4))
             (hash (+ (ash (aref digest base) 24)
                      (ash (aref digest (+ base 1)) 16)
                      (ash (aref digest (+ base 2)) 8)
                      (aref digest (+ base 3))))
             (bit (mod hash (* 8 (length bloom))))
             (byte (floor bit 8)))
        (setf (aref bloom byte)
              (logior (aref bloom byte) (ash 1 (mod bit 8)))))))
  bloom)

(defun %s3-sidecar-bloom (keys)
  (let ((bloom (make-array 32 :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (dolist (key keys bloom) (%s3-bloom-add bloom key))))

(defun %s3-index-sidecar-write (cfg relative entries)
  (let ((path (%s3-index-sidecar-path cfg relative)))
    (when path
      (ensure-directories-exist path)
      (let ((tmp (format nil "~a.tmp.~d.~d" path (get-universal-time)
                         (random 1000000000))))
        (with-open-file (out tmp :direction :output :if-exists :supersede
                                  :external-format :utf-8)
          (with-standard-io-syntax
            (let* ((keys (mapcar #'first entries))
                   (sorted (sort (copy-list keys) #'string<)))
              (prin1 (list :version 2 :segment relative :count (length keys)
                           :key-range (list (first sorted) (car (last sorted)))
                           :bloom (%s3-sidecar-bloom keys)) out)
              (terpri out))
            (dolist (entry entries)
              (prin1 (list (first entry) (third entry) (fourth entry)
                           (fifth entry)) out)
              (terpri out))))
        (rename-file tmp path)))))

(defun %s3-index-sidecar-map (cfg relative fn)
  (let ((path (%s3-index-sidecar-path cfg relative)))
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (in path :external-format :utf-8)
            (let ((header (with-standard-io-syntax (read in nil nil))))
              (when (and (listp header) (member (getf header :version) '(1 2))
                         (equal (getf header :segment) relative))
                (let ((count 0) (keys nil))
                  (loop for line = (read-line in nil nil)
                        while line
                        do (let ((entry (with-standard-io-syntax
                                          (car (multiple-value-list
                                                (read-from-string line))))))
                             (push (first entry) keys)
                             (incf count)
                             (funcall fn (list (first entry) relative
                                                (second entry) (third entry)
                                                (fourth entry)))))
                  (or (and (eql (getf header :version) 2)
                           (= count (getf header :count))
                           (equal (getf header :key-range)
                                  (let ((sorted (sort (copy-list keys) #'string<)))
                                    (list (first sorted) (car (last sorted)))))
                           (equal (getf header :bloom) (%s3-sidecar-bloom keys)))
                      (eql (getf header :version) 1))))))
        (error () nil)))))

(defun %s3-summary-path (cfg)
  (%s3-meta-path cfg
                 (format nil "lazy-summary-~a.sexp"
                         (%s3-hex (%s3-sha256
                                   (string-to-octets
                                    (format nil "~a/~a" (s3-config-bucket cfg)
                                            (s3-config-prefix cfg))))))))

(defun %s3-hash-pairs (table)
  (loop for key being the hash-keys of table using (hash-value value)
        collect (cons key value)))

(defun %s3-nested-pairs (table)
  (loop for key being the hash-keys of table using (hash-value value)
        collect (cons key (%s3-hash-pairs value))))

(defun %s3-summary-save (cfg ordinary segments)
  (when (and (s3-config-summary-valid cfg) (s3-config-cache-dir cfg))
    (%s3-write-sexp
     (%s3-summary-path cfg)
     (list :version 1 :ordinary (sort (copy-list ordinary) #'string<)
           :segments (sort (copy-list segments) #'string<)
           :counts (%s3-hash-pairs (s3-config-lazy-counts cfg))
           :labels (%s3-nested-pairs (s3-config-lazy-labels cfg))
           :types (%s3-nested-pairs (s3-config-lazy-type-counts cfg))
           :sums (%s3-nested-pairs (s3-config-lazy-sums cfg))
           :topk (%s3-nested-pairs (s3-config-lazy-topk-summaries cfg))))))

(defun %s3-summary-load (cfg ordinary segments)
  (let ((value (%s3-read-sexp (%s3-summary-path cfg))))
    (when (and (listp value) (eql (getf value :version) 1)
               (equal (getf value :ordinary) (sort (copy-list ordinary) #'string<))
               (equal (getf value :segments) (sort (copy-list segments) #'string<)))
      (labels ((restore (target pairs)
                 (dolist (pair pairs) (setf (gethash (car pair) target) (cdr pair))))
               (restore-nested (target pairs)
                 (dolist (pair pairs)
                   (let ((index (make-hash-table :test #'equal)))
                     (restore index (cdr pair))
                     (setf (gethash (car pair) target) index)))))
        (restore (s3-config-lazy-counts cfg) (getf value :counts))
        (restore-nested (s3-config-lazy-labels cfg) (getf value :labels))
        (restore-nested (s3-config-lazy-type-counts cfg) (getf value :types))
        (restore-nested (s3-config-lazy-sums cfg) (getf value :sums))
        (restore-nested (s3-config-lazy-topk-summaries cfg) (getf value :topk))
        (setf (s3-config-summary-valid cfg) t)
        t))))

(defun %s3-cache-invalidate (cfg relative)
  (let ((path (%s3-cache-file cfg relative)))
    (when (and path (probe-file path))
      (ignore-errors (delete-file path)))))

(defun %s3-cache-enforce-budget (cfg required path)
  (let ((limit (s3-config-cache-max-bytes cfg)))
    (when (and (plusp limit) (<= required limit))
      (labels ((size (file)
                 (ignore-errors
                   (with-open-file (in file :element-type '(unsigned-byte 8))
                     (file-length in)))))
        (let* ((dir (s3-config-cache-dir cfg))
               (files (and dir (directory (merge-pathnames "*.bin" dir))))
               (total (loop for f in files when (probe-file f) sum (or (size f) 0))))
          (dolist (file (sort (remove path files :test #'equal)
                              #'< :key (lambda (f) (or (file-write-date f) 0))))
            (when (<= (+ total required) limit) (return))
            (let ((bytes (size file)))
              (when (and bytes (ignore-errors (delete-file file)))
                (decf total bytes)))))))))

(defun %s3-valid-packed-segment-p (relative bytes)
  (handler-case
      (let ((key (%s3-unhex-key relative)))
        (if (and (>= (length key) 7) (string= key "@batch:" :end1 7))
            (multiple-value-bind (outer pos) (read-u32 bytes 1)
              (declare (ignore outer))
              (multiple-value-bind (magic ignored) (%codec-read bytes pos)
                (declare (ignore ignored))
                (string= magic "scalaxy-s3-batch-v1")))
            t))
    (error () nil)))

(defun %s3-get-cached (cfg relative)
  "Read an object through the local persistent cache and self-heal bad segments."
  (let ((path (%s3-cache-file cfg relative)) (cached nil))
    (when (and path (probe-file path))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence v in)
          (when (%s3-valid-packed-segment-p relative v)
            (setf cached v)))))
    (if cached
        (progn
          (incf (s3-config-cache-hits cfg))
          (incf (s3-config-cache-bytes cfg) (length cached))
          cached)
        (progn
          (when (and path (probe-file path)) (ignore-errors (delete-file path)))
          (incf (s3-config-cache-misses cfg))
          (multiple-value-bind (status headers body)
              (%s3-call cfg "GET" relative :binary t)
            (declare (ignore headers))
            (unless (= status 200) (error "S3 GET failed with HTTP ~d" status))
            (let ((bytes (%s3-decrypt-body
                          (if (typep body '(vector (unsigned-byte 8))) body
                              (string-to-octets body)))))
              (incf (s3-config-cache-bytes cfg) (length bytes))
              (when (and path
                         (or (zerop (s3-config-cache-max-bytes cfg))
                             (<= (length bytes) (s3-config-cache-max-bytes cfg))))
                (%s3-cache-enforce-budget cfg (length bytes) path)
                (ensure-directories-exist path)
                (let ((tmp (format nil "~a.tmp.~d.~d" path (get-universal-time)
                                   (random 1000000000))))
                  (with-open-file (out tmp :direction :output :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
                    (write-sequence bytes out))
                  (rename-file tmp path)))
              bytes))))))

(defun %s3-apply-segments-parallel (cfg segments table)
  "Fetch/decode independent immutable segments in parallel, then merge in
sequence order so overwrite/delete semantics remain deterministic."
  (let ((workers 4) (n (length segments)))
    (loop for start from 0 below n by workers
          for batch = (subseq segments start (min n (+ start workers)))
          do
            #+sbcl
            (let ((results (make-array (length batch))) (threads nil))
              (loop for relative in batch for j from 0
                    do (let ((index j) (rel relative))
                         (push (sb-thread:make-thread
                                (lambda ()
                                  (let ((ht (make-hash-table :test #'equal)))
                                    (%s3-apply-batch-bytes (%s3-get-cached cfg rel) ht)
                                    (setf (aref results index) ht))))
                               threads)))
              (dolist (thread threads) (sb-thread:join-thread thread))
              (dotimes (j (length batch))
                (maphash (lambda (key value) (setf (gethash key table) value))
                         (aref results j))))
            #-sbcl
            (dolist (relative batch)
              (%s3-apply-batch-bytes (%s3-get-cached cfg relative) table)))))

(defun %s3-lazy-label-key (cfg key value-bytes)
  (when (and (>= (length key) 4) (string= key "d:" :end1 2))
    (let ((sep (position #\: key :start 2)))
      (when sep
        (let ((local (subseq key (1+ sep))))
          (when (and (>= (length local) 2) (string= local "n:" :end1 2))
            (handler-case
                (let* ((record-bytes (car (multiple-value-list (codec-decode value-bytes))))
                       (record (and (vectorp record-bytes)
                                    (car (multiple-value-list (codec-decode record-bytes))))))
                  (when (cypher-map-p record)
                    (dolist (label (cypher-list-elements
                                    (cdr (assoc "labels" (cypher-map-pairs record)
                                                 :test #'equal))))
                (let* ((db (subseq key 2 sep))
                       (labels (or (gethash db (s3-config-lazy-labels cfg))
                                   (setf (gethash db (s3-config-lazy-labels cfg))
                                         (make-hash-table :test #'equal))))
                       (by-label (or (gethash db (s3-config-lazy-label-ids cfg))
                                     (setf (gethash db (s3-config-lazy-label-ids cfg))
                                           (make-hash-table :test #'equal))))
                       (ids (or (gethash label by-label)
                                (setf (gethash label by-label)
                                      (make-hash-table :test #'equal)))))
                  (incf (gethash label labels 0))
                  (setf (gethash (subseq local 2) ids) t))))))))))))

(defun %s3-label-id-path (cfg)
  (%s3-meta-path cfg
                 (format nil "lazy-label-ids-~a.sexp"
                         (%s3-hex (%s3-sha256
                                   (string-to-octets
                                    (format nil "~a/~a" (s3-config-bucket cfg)
                                            (s3-config-prefix cfg))))))))

(defun %s3-label-id-save (cfg ordinary segments)
  (when (and (s3-config-summary-valid cfg) (s3-config-cache-dir cfg))
    (%s3-write-sexp (%s3-label-id-path cfg)
                    (list :version 1
                          :ordinary (sort (copy-list ordinary) #'string<)
                          :segments (sort (copy-list segments) #'string<)
                          :ids (loop for db being the hash-keys of
                                                   (s3-config-lazy-label-ids cfg)
                                     using (hash-value labels)
                                     collect (cons db
                                                   (loop for label being the hash-keys of labels
                                                         using (hash-value ids)
                                                         collect (cons label (%s3-hash-pairs ids)))))))))

(defun %s3-label-id-load (cfg ordinary segments)
  (let ((value (%s3-read-sexp (%s3-label-id-path cfg))))
    (when (and (listp value) (eql (getf value :version) 1)
               (equal (getf value :ordinary) (sort (copy-list ordinary) #'string<))
               (equal (getf value :segments) (sort (copy-list segments) #'string<)))
      (dolist (db-pair (getf value :ids))
        (let ((labels (make-hash-table :test #'equal)))
          (dolist (label-pair (cdr db-pair))
            (let ((ids (make-hash-table :test #'equal)))
              (dolist (id-pair (cdr label-pair)) (setf (gethash (car id-pair) ids) t))
              (setf (gethash (car label-pair) labels) ids)))
          (setf (gethash (car db-pair) (s3-config-lazy-label-ids cfg)) labels)))
      t)))

(defun %s3-remove-lazy-label-id (cfg key)
  "Remove a deleted node id from every persisted label summary."
  (when (and (>= (length key) 4) (string= key "d:" :end1 2))
    (let ((sep (position #\: key :start 2)))
      (when sep
        (let ((local (subseq key (1+ sep))))
          (when (and (>= (length local) 2) (string= local "n:" :end1 2))
            (let* ((db (subseq key 2 sep))
                   (id (subseq local 2))
                   (labels (gethash db (s3-config-lazy-label-ids cfg))))
              (when labels
                (loop for label being the hash-values of labels
                      do (remhash id label))))))))))

(defun %s3-endpoint-aggregate-path (cfg)
  (%s3-meta-path cfg
                 (format nil "lazy-endpoint-aggregates-~a.sexp"
                         (%s3-hex (%s3-sha256
                                   (string-to-octets
                                    (format nil "~a/~a" (s3-config-bucket cfg)
                                            (s3-config-prefix cfg))))))))

(defun %s3-endpoint-aggregate-save (cfg segments)
  (let ((table (s3-config-lazy-endpoint-aggregates cfg)))
    (when (and (s3-config-summary-valid cfg)
               (s3-config-cache-dir cfg) (not (gethash :disabled table)))
      (%s3-write-sexp (%s3-endpoint-aggregate-path cfg)
                      (list :version 4
                            :segments (sort (copy-list segments) #'string<)
                            :values (%s3-hash-pairs table))))))

(defun %s3-endpoint-aggregate-load (cfg segments)
  (let ((value (%s3-read-sexp (%s3-endpoint-aggregate-path cfg))))
    (when (and (listp value) (eql (getf value :version) 4)
               (equal (getf value :segments) (sort (copy-list segments) #'string<)))
      (dolist (pair (getf value :values))
        (setf (gethash (car pair) (s3-config-lazy-endpoint-aggregates cfg))
              (cdr pair)))
      t)))

(defun %s3-endpoint-aggregate-add (cfg key bytes)
  (let ((table (s3-config-lazy-endpoint-aggregates cfg))
        (sep (position #\: key :start 2)))
    (when (and sep (not (gethash :disabled table)))
      (let ((local (subseq key (1+ sep))))
        (when (and (>= (length local) 2) (string= local "r:" :end1 2))
          ;; Decode the record fully so aggregation matches query-time
          ;; semantics exactly; the light parser misses rare encodings.
          (handler-case
              (let* ((raw (car (multiple-value-list (%codec-read bytes 0))))
                     (recm (%decode-record raw))
                     (type (%record-get recm "type"))
                     (start (%record-get recm "start"))
                     (end (%record-get recm "end")))
                (when (and type start end)
                  (let* ((db (subseq key 2 sep))
                         (aggregate-key (list db (princ-to-string type)
                                              (princ-to-string start)
                                              (princ-to-string end)))
                         (values (or (gethash aggregate-key table)
                                     (if (>= (hash-table-count table) 200000)
                                         (progn
                                           (setf (gethash :disabled table) t)
                                           nil)
                                         (let ((v (make-hash-table :test #'equal)))
                                           (setf (gethash aggregate-key table) v)
                                           v)))))
                    (when values
                      (incf (gethash "~count" values 0))
                      (dolist (pair (%props-of recm))
                        (let ((value (cdr pair)))
                          (when (and (numberp value) (not (eq value :cypher-null)))
                            (incf (gethash (car pair) values 0) value))))))))
            (error () nil)))))))

(defun %s3-index-endpoint-aggregates (cfg relative &optional skip-check)
  "Aggregate relationship records from one packed segment.
When SKIP-CHECK is T, process all PUT records without checking the
lazy-index (used in streaming mode where no index is built)."
  (let ((bytes (%s3-get-cached cfg relative)))
    (multiple-value-bind (outer pos) (read-u32 bytes 1)
      (declare (ignore outer))
      (multiple-value-bind (magic next) (%codec-read bytes pos)
        (declare (ignore magic))
        (when (= (aref bytes next) +tag-list+)
          (multiple-value-bind (count cursor0) (read-u32 bytes (1+ next))
            (let ((cursor cursor0))
              (loop repeat count
                    do (when (= (aref bytes cursor) +tag-list+)
                         (multiple-value-bind (fields p0) (read-u32 bytes (1+ cursor))
                           (declare (ignore fields))
                           (multiple-value-bind (op p1) (%codec-read bytes p0)
                             (multiple-value-bind (key p2) (%codec-read bytes p1)
                               (let ((after (%codec-skip bytes p2)))
                                 (when (string= op "PUT")
                                   (when (or skip-check
                                             (let ((entry (gethash key (s3-config-lazy-index cfg))))
                                               (and entry (equal relative (first entry))
                                                    (= p2 (second entry)))))
                                     (%s3-endpoint-aggregate-add cfg key
                                                                 (subseq bytes p2 after))))
                                 (setf cursor after))))))))))))))
(defun %s3-lazy-count-key (cfg key delta)
  (when (and (>= (length key) 4) (string= key "d:" :end1 2))
    (let ((sep (position #\: key :start 2)))
      (when sep
        (let* ((db (subseq key 2 sep)) (local (subseq key (1+ sep)))
               (kind (cond ((and (>= (length local) 2) (string= local "n:" :end1 2)) :nodes)
                           ((and (>= (length local) 2) (string= local "r:" :end1 2)) :rels))))
          (when kind
            (let ((counts (or (gethash db (s3-config-lazy-counts cfg))
                              (setf (gethash db (s3-config-lazy-counts cfg)) (cons 0 0)))))
              (if (eq kind :nodes) (incf (car counts) delta)
                  (incf (cdr counts) delta)))))))))

(defun %s3-lazy-topk-add (cfg db type property rid value)
  (let* ((db-index (or (gethash db (s3-config-lazy-topk-summaries cfg))
                       (setf (gethash db (s3-config-lazy-topk-summaries cfg))
                             (make-hash-table :test #'equal))))
         (pair (cons value rid)))
    (dolist (direction '(:asc :desc))
      (let* ((key (list type property direction))
             (values (cons pair (gethash key db-index))))
        (setf (gethash key db-index)
              (subseq (sort values (if (eq direction :asc) #'< #'>) :key #'car)
                      0 (min 100 (length values))))))))

(defun %s3-lazy-type-key (cfg key bytes)
  (when (and (>= (length key) 4) (string= key "d:" :end1 2))
    (let ((sep (position #\: key :start 2)))
      (when sep
        (let* ((db (subseq key 2 sep))
               (local (subseq key (1+ sep)))
               (counts (or (gethash db (s3-config-lazy-type-counts cfg))
                           (setf (gethash db (s3-config-lazy-type-counts cfg))
                                 (make-hash-table :test #'equal)))))
          (when (and (>= (length local) 2) (string= local "r:" :end1 2))
            (let* ((record (if (and (vectorp bytes) (= (aref bytes 0) +tag-bytes+))
                               (car (multiple-value-list (%codec-read bytes 0)))
                               bytes))
                   (type (%codec-map-field-light record "type"))
                   (props (%codec-map-field-light record "props"))
                   (rid (subseq local 2)))
              (when type
                (incf (gethash type counts 0))
                (when (cypher-map-p props)
                  (dolist (pair (cypher-map-pairs props))
                    (when (numberp (cdr pair))
                      (let* ((key (list type (car pair)))
                             (sums (or (gethash db (s3-config-lazy-sums cfg))
                                       (setf (gethash db (s3-config-lazy-sums cfg))
                                             (make-hash-table :test #'equal)))))
                        (incf (gethash key sums 0) (cdr pair))
                        (%s3-lazy-topk-add cfg db type (car pair) rid (cdr pair))))))))))))))

(defun %s3-index-batch (cfg relative &optional (build-metadata t))
  "Index keys in a packed segment without decoding record values."
  (let ((bytes (%s3-get-cached cfg relative)) (out nil))
    (multiple-value-bind (outer pos) (read-u32 bytes 1)
      (declare (ignore outer))
      (multiple-value-bind (magic next) (%codec-read bytes pos)
        (unless (string= magic "scalaxy-s3-batch-v1") (return-from %s3-index-batch nil))
        (unless (= (aref bytes next) +tag-list+) (return-from %s3-index-batch nil))
        (multiple-value-bind (count cursor0) (read-u32 bytes (1+ next))
          (let ((cursor cursor0))
            (loop repeat count
                  do (unless (= (aref bytes cursor) +tag-list+)
                       (return))
                     (multiple-value-bind (fields p0) (read-u32 bytes (1+ cursor))
                       (declare (ignore fields))
                       (multiple-value-bind (op p1) (%codec-read bytes p0)
                         (multiple-value-bind (key p2) (%codec-read bytes p1)
                           (if (string= op "PUT")
                               (let ((after (%codec-skip bytes p2)))
                                 (when build-metadata
                                   (%s3-lazy-type-key cfg key (subseq bytes p2 after)))
                                 (when (and build-metadata
                                            (>= (length key) 4) (string= key "d:" :end1 2))
                                   (%s3-lazy-label-key cfg key (subseq bytes p2 after)))
                                 (push (list key relative p2 after) out)
                                 (setf cursor after))
                               (progn
                                 (push (list key relative nil nil :delete) out)
                                 (setf cursor p2)))))))))))
    (let ((result (nreverse out)))
      (%s3-index-sidecar-write cfg relative result)
      result)))

(defun %s3-sidecar-might-contain-p (cfg relative key)
  "Check if RELATIVE's sidecar Bloom filter might contain KEY.
Returns :yes (might contain) or :no (definitely not)."
  (let ((path (%s3-index-sidecar-path cfg relative)))
    (when (and path (probe-file path))
      (handler-case
          (with-open-file (in path :element-type '(unsigned-byte 8))
            ;; Read enough bytes for the header (magic + version + segment name + count + range + bloom)
            (let* ((header-size 512)
                   (buf (make-array (min header-size (file-length in))
                                    :element-type '(unsigned-byte 8))))
              (read-sequence buf in)
              ;; Parse the sexp header from the raw bytes
              (let ((header-str (map 'string #'code-char
                                     (remove-if (lambda (b) (> b 127)) buf))))
                (when (search "SCX1" header-str)
                  :maybe)
                ;; Simple check: if we can read it as a sexp, look for bloom
                (let ((form (ignore-errors
                             (with-input-from-string (s (map 'string #'code-char buf))
                               (read s nil nil)))))
                  (when (and (listp form) (eql (getf form :version) 2))
                    ;; Valid v2 sidecar - check bloom against key
                    (let ((bloom (getf form :bloom)))
                      (if bloom
                          ;; Recompute what the key would add and compare
                          (let ((test-bloom (%s3-bloom-add
                                             (make-array 32 :element-type '(unsigned-byte 8)
                                                              :initial-element 0)
                                             key)))
                            ;; If OR of test-bloom into stored bloom changes nothing,
                            ;; the key is already represented
                            (if (equalp test-bloom
                                        (subseq test-bloom 0 32))
                                :maybe
                                :no))
                      :no)))))))
        (error () :no)))))

(defun %s3-load-lazy (cfg table)
  "Load ordinary objects and build a deterministic packed-segment index."
  (let ((marker nil) (objects nil))
    (loop
      (let ((query (format nil "list-type=2&max-keys=1000&prefix=~a~:[~;&start-after=~a~]"
                           (%s3-url-encode (s3-config-prefix cfg))
                           marker
                           (and marker (%s3-url-encode marker)))))
        (multiple-value-bind (status headers body) (%s3-call cfg "GET" "" :query query)
          (declare (ignore headers))
          (unless (= status 200) (error "S3 LIST failed with HTTP ~d: ~a" status body))
          (let ((page-keys nil))
            (dolist (object (%s3-xml-values body "Key"))
              (when (search (s3-config-prefix cfg) object)
                (push (subseq object (length (s3-config-prefix cfg))) page-keys)))
            (setf page-keys (nreverse page-keys))
            (dolist (k page-keys) (push k objects))
            (unless (>= (length page-keys) 1000) (return))
            (setf marker (car (last page-keys)))))))
    (let ((ordinary nil) (segments nil) (tombstones nil))
      (dolist (relative objects)
        (let ((key (%s3-unhex-key relative)))
          (cond
            ((and (>= (length key) 7) (string= key "@batch:" :end1 7))
             (push relative segments)
             (setf (gethash relative (s3-config-lazy-segments cfg)) t))
            ((and (>= (length key) 11) (string= key "@tombstone:" :end1 11))
             (push key tombstones))
            (t (push (cons key relative) ordinary)))))
      (when (s3-config-streaming-mode cfg)
        ;; Streaming mode: accumulate metadata (counts, types, sums,
        ;; label-IDs, endpoint-pairs) without building the O(N) lazy-index.
        ;; Point reads fall through to direct S3 GET.
        (dolist (relative segments)
          (%s3-index-batch cfg relative t)
          (%s3-index-endpoint-aggregates cfg relative t))
        (setf (s3-config-summary-valid cfg) t)
        (return-from %s3-load-lazy table))
      (let* ((ordinary-relative (mapcar #'cdr ordinary))
             (summary-loaded
              (and (%s3-summary-load cfg ordinary-relative segments)
                   (%s3-label-id-load cfg ordinary-relative segments)))
             (endpoint-loaded
              (%s3-endpoint-aggregate-load cfg segments)))
        (dolist (pair ordinary)
          (setf (gethash (car pair) table)
                (car (multiple-value-list
                      (codec-decode (%s3-get-cached cfg (cdr pair)))))))
        (labels ((apply-entry (entry)
                   (let ((key (first entry)) (delete-p (eq (fifth entry) :delete)))
                     (when (and (not summary-loaded)
                                (gethash key (s3-config-lazy-index cfg)))
                       (setf (s3-config-summary-valid cfg) nil))
                     (if delete-p
                         (progn
                           (%s3-remove-lazy-label-id cfg key)
                           (when (and (not summary-loaded)
                                      (gethash key (s3-config-lazy-index cfg)))
                             (%s3-lazy-count-key cfg key -1))
                           (remhash key (s3-config-lazy-index cfg))
                           (remhash key table)
                           (unless summary-loaded
                             (setf (s3-config-summary-valid cfg) nil)))
                         (progn
                           (when (and (not summary-loaded)
                                      (not (gethash key (s3-config-lazy-index cfg))))
                             (%s3-lazy-count-key cfg key 1))
                           (remhash key table)
                           (setf (gethash key (s3-config-lazy-index cfg))
                                 (cdr entry)))))))
          (dolist (relative (sort segments #'string<))
            (let ((used nil))
              (when summary-loaded
                (setf used (%s3-index-sidecar-map
                            cfg relative #'apply-entry)))
              (unless used
                (dolist (entry (%s3-index-batch cfg relative (not summary-loaded)))
                  (apply-entry entry)))))
          (dolist (marker tombstones)
            (let ((sep (position #\: marker :start 11)))
              (when sep
                (let ((key (%s3-unhex-key (subseq marker 11 sep))))
                  (when (gethash key (s3-config-lazy-index cfg))
                    (%s3-lazy-count-key cfg key -1))
                  (remhash key (s3-config-lazy-index cfg))
                  (remhash key table)
                  (unless summary-loaded
                    (setf (s3-config-summary-valid cfg) nil))))))
          (unless endpoint-loaded
            (when summary-loaded (clrhash (s3-config-lazy-endpoint-aggregates cfg)))
            (dolist (relative (sort segments #'string<))
              (%s3-index-endpoint-aggregates cfg relative)))
          ;; A fresh rebuild produces exactly current summaries: mark them
          ;; valid so they persist for the next start.
          (unless summary-loaded
            (setf (s3-config-summary-valid cfg) t))
          ;; A fresh rebuild produces exactly current summaries.
          (unless summary-loaded
            (setf (s3-config-summary-valid cfg) t))
          (%s3-endpoint-aggregate-save cfg segments)
          (%s3-summary-save cfg (mapcar #'cdr ordinary) segments)
          (%s3-label-id-save cfg (mapcar #'cdr ordinary) segments)
          (%s3-aggregate-cache-load cfg segments)
          table)))))

(defun %s3-get-cached-range (cfg relative start end)
  "Read one immutable packed value from the persistent cache when present."
  (let ((path (%s3-cache-file cfg relative)))
    (if (and path (probe-file path))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (if (>= (file-length in) end)
              (let ((v (make-array (- end start) :element-type '(unsigned-byte 8))))
                (incf (s3-config-cache-hits cfg))
                (file-position in start)
                (read-sequence v in)
                (incf (s3-config-cache-bytes cfg) (length v))
                (car (multiple-value-list (%codec-read v 0))))
              (progn
                (%s3-cache-invalidate cfg relative)
                (let ((bytes (%s3-get-cached cfg relative)))
                  (car (multiple-value-list (%codec-read bytes start)))))))
        (let ((bytes (%s3-get-cached cfg relative)))
          (car (multiple-value-list (%codec-read bytes start)))))))

(defun %s3-lazy-get (cfg key)
  "Read the value for KEY, using the lazy-index or direct S3 GET in streaming mode."
  (let ((index (s3-config-lazy-index cfg)))
    (if (and index (plusp (hash-table-count index)))
        (let ((entry (gethash key index)))
          (when entry
            (let ((rel (first entry)) (start (second entry)) (end (third entry)))
              (when (and start end)
                (%s3-get-cached-range cfg rel start end)))))
        (multiple-value-bind (status headers body)
            (%s3-call cfg "GET" (%s3-hex-key key) :binary t)
          (declare (ignore headers))
          (when (= status 200) (%s3-decrypt-body body))))))

(defun %s3-load (cfg table &optional decoder)
  (when (and (s3-config-lazy cfg) (null decoder))
    (return-from %s3-load (%s3-load-lazy cfg table)))
  (setf decoder (or decoder (lambda (bytes) (car (multiple-value-list (codec-decode bytes))))))
  "Load ordinary objects and then bulk mutation segments.
Segments are applied last so they can represent updates/deletes of older
individual objects while retaining deterministic restart semantics."
  (let ((marker nil) (objects nil))
    (loop
      (let ((query (format nil "list-type=2&max-keys=1000&prefix=~a~:[~;&start-after=~a~]"
                           (%s3-url-encode (s3-config-prefix cfg))
                           marker
                           (and marker (%s3-url-encode marker)))))
        (multiple-value-bind (status headers body) (%s3-call cfg "GET" "" :query query)
          (declare (ignore headers))
          (unless (= status 200) (error "S3 LIST failed with HTTP ~d: ~a" status body))
          (let ((page-keys nil))
            (dolist (object (%s3-xml-values body "Key"))
              (when (search (s3-config-prefix cfg) object)
                (push (subseq object (length (s3-config-prefix cfg))) page-keys)))
            (setf page-keys (nreverse page-keys))
            (dolist (k page-keys) (push k objects))
            (unless (>= (length page-keys) 1000) (return))
            (setf marker (car (last page-keys)))))))
    (let ((segments nil) (tombstones nil))
      (dolist (relative objects)
        (let ((key (%s3-unhex-key relative)))
          (cond
            ((and (>= (length key) 7) (string= key "@batch:" :end1 7))
             (push relative segments))
            ((and (>= (length key) 11) (string= key "@tombstone:" :end1 11))
             (push key tombstones))
            (t
             (setf (gethash key table)
                   (funcall decoder (%s3-get-cached cfg relative))))))
      (%s3-apply-segments-parallel cfg (sort segments #'string<) table)))
      (dolist (marker tombstones)
        (let ((sep (position #\: marker :start 11)))
          (when sep
            (remhash (%s3-unhex-key (subseq marker 11 sep)) table))))))

(defun %s3-aggregate-cache-path (cfg)
  (%s3-meta-path cfg
                 (format nil "lazy-aggregate-cache-~a.sexp"
                         (%s3-hex (%s3-sha256
                                   (string-to-octets
                                    (format nil "~a/~a" (s3-config-bucket cfg)
                                            (s3-config-prefix cfg))))))))

(defun %s3-aggregate-cache-save (cfg)
  (when (and (s3-config-summary-valid cfg) (s3-config-cache-dir cfg))
    (%s3-write-sexp (%s3-aggregate-cache-path cfg)
                    (list :version 1
                          :segments (sort (loop for key being the hash-keys
                                                       of (s3-config-lazy-segments cfg)
                                                 collect key)
                                          #'string<)
                          :values (%s3-hash-pairs
                                   (s3-config-lazy-aggregate-cache cfg))))))

(defun %s3-aggregate-cache-load (cfg segments)
  (let ((value (%s3-read-sexp (%s3-aggregate-cache-path cfg))))
    (when (and (listp value) (eql (getf value :version) 1)
               (equal (getf value :segments) (sort (copy-list segments) #'string<)))
      (dolist (pair (getf value :values))
        (setf (gethash (car pair) (s3-config-lazy-aggregate-cache cfg))
              (cdr pair)))
      t)))

(defun %s3-delete (cfg key)
  (let ((encoded (%s3-hex-key key)))
    (multiple-value-bind (status headers body) (%s3-call cfg "DELETE" encoded)
      (declare (ignore headers body))
      (unless (member status '(200 204)) (error "S3 DELETE failed with HTTP ~d" status)))
    (%s3-cache-invalidate cfg encoded)
    (%s3-lazy-unindex-key cfg key)
    (%s3-clear-aggregate-cache cfg key)
    ;; A tombstone prevents an older packed import segment from resurrecting
    ;; this key when the store is reconstructed after restart.
    (let ((marker (format nil "@tombstone:~a:~d" encoded (get-universal-time))))
      (multiple-value-bind (status headers body)
          (%s3-call cfg "PUT" (%s3-hex-key marker)
                    :body (make-array 0 :element-type '(unsigned-byte 8)))
        (declare (ignore headers))
        (unless (member status '(200 201 204))
          (error "S3 tombstone PUT failed with HTTP ~d: ~a" status body))))))

(defun %s3-lazy-unindex-key (cfg key)
  "Immediately remove KEY from the in-memory lazy indexes after a delete."
  (when (gethash key (s3-config-lazy-index cfg))
    (%s3-lazy-count-key cfg key -1)
    (%s3-remove-lazy-label-id cfg key)
    (remhash key (s3-config-lazy-index cfg)))
  (setf (s3-config-summary-valid cfg) nil))

(defun %s3-clear-aggregate-cache (cfg &optional affected-key)
  "Invalidate cached aggregates.  When AFFECTED-KEY is given, only
relationship-affecting mutations clear the expensive per-type and
per-endpoint summaries."
  (when (s3-config-lazy-aggregate-cache cfg)
    (clrhash (s3-config-lazy-aggregate-cache cfg)))
  (let ((rel-p (and affected-key
                    (> (length affected-key) 4)
                    (string= affected-key "d:" :end1 2)
                    (let ((sep (position #\: affected-key :start 2)))
                      (and sep
                           (> (length affected-key) (+ sep 2))
                           (string= affected-key "r:" :start1 (+ sep 2) :end1 (+ sep 4)))))))
    (when rel-p
      (setf (s3-config-summary-valid cfg) nil)
      (when (s3-config-lazy-endpoint-aggregates cfg)
        (clrhash (s3-config-lazy-endpoint-aggregates cfg)))
      (dolist (path (list (%s3-summary-path cfg)
                          (%s3-label-id-path cfg)
                          (%s3-endpoint-aggregate-path cfg)
                          (%s3-aggregate-cache-path cfg)))
        (when (and path (probe-file path)) (ignore-errors (delete-file path)))))))
(defun %s3-put-batch (cfg records)
  "Write a bulk mutation segment as one S3 object.
RECORDS contains (OP KEY VALUE), where OP is the string PUT or DELETE."
  (when records
    (let* ((id (format nil "@batch:~d-~d-~d" (get-universal-time)
                       (get-internal-real-time) (incf *s3-batch-sequence*)))
           (payload (%s3-encrypt-body (codec-encode (list "scalaxy-s3-batch-v1" records)))))
      (multiple-value-bind (status headers body)
          (%s3-call cfg "PUT" (%s3-hex-key id) :body payload)
        (declare (ignore headers))
        (unless (member status '(200 201 204))
          (error "S3 batch PUT failed with HTTP ~d: ~a" status body))
        (%s3-clear-aggregate-cache cfg)))))

(defun %s3-put-raw (cfg key bytes)
  (multiple-value-bind (status headers body) (%s3-call cfg "PUT" (%s3-hex-key key) :body (%s3-encrypt-body bytes))
    (declare (ignore headers))
    (unless (member status '(200 201 204))
      (error "S3 PUT failed with HTTP ~d: ~a" status body))
    (%s3-cache-invalidate cfg (%s3-hex-key key))
    (%s3-clear-aggregate-cache cfg key)))

(defun %s3-put (cfg key value)
  (multiple-value-bind (status headers body) (%s3-call cfg "PUT" (%s3-hex-key key) :body (%s3-encrypt-body (codec-encode value)))
    (declare (ignore headers))
    (unless (member status '(200 201 204))
      (error "S3 PUT failed with HTTP ~d: ~a" status body))
    (%s3-cache-invalidate cfg (%s3-hex-key key))
    (%s3-clear-aggregate-cache cfg key)))
