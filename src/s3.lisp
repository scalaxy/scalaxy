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

(defstruct (s3-config (:constructor %make-s3-config (endpoint host port bucket access-key secret-key region prefix cache-dir lazy lazy-index lazy-segments lazy-counts lazy-labels lazy-type-counts lazy-sums summary-valid cache-hits cache-misses cache-bytes)))
  endpoint host port bucket access-key secret-key region prefix cache-dir lazy lazy-index lazy-segments lazy-counts lazy-labels lazy-type-counts lazy-sums summary-valid cache-hits cache-misses cache-bytes)

(defun make-s3-config (&key endpoint bucket access-key secret-key (region "us-east-1") (prefix "scalaxy/") cache-dir lazy)
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
                     lazy (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     (and lazy (make-hash-table :test #'equal))
                     t 0 0 0)))

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

(defun %s3-cache-invalidate (cfg relative)
  (let ((path (%s3-cache-file cfg relative)))
    (when (and path (probe-file path))
      (ignore-errors (delete-file path)))))

(defun %s3-get-cached (cfg relative)
  "Read an object through the local persistent cache and record metrics."
  (let ((path (%s3-cache-file cfg relative)))
    (if (and path (probe-file path))
        (progn
          (incf (s3-config-cache-hits cfg))
          (with-open-file (in path :element-type '(unsigned-byte 8))
            (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
              (read-sequence v in)
              (incf (s3-config-cache-bytes cfg) (length v))
              v)))
        (progn
          (incf (s3-config-cache-misses cfg))
          (multiple-value-bind (status headers body)
              (%s3-call cfg "GET" relative :binary t)
            (declare (ignore headers))
            (unless (= status 200) (error "S3 GET failed with HTTP ~d" status))
            (let ((bytes (if (typep body '(vector (unsigned-byte 8))) body
                             (string-to-octets body))))
              (incf (s3-config-cache-bytes cfg) (length bytes))
              (when path
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
                                         (make-hash-table :test #'equal)))))
                  (incf (gethash label labels 0)))))))))))))

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
                   (props (%codec-map-field-light record "props")))
              (when type
                (incf (gethash type counts 0))
                (when (cypher-map-p props)
                  (dolist (pair (cypher-map-pairs props))
                    (when (numberp (cdr pair))
                      (let* ((key (list type (car pair)))
                             (sums (or (gethash db (s3-config-lazy-sums cfg))
                                       (setf (gethash db (s3-config-lazy-sums cfg))
                                             (make-hash-table :test #'equal)))))
                        (incf (gethash key sums 0) (cdr pair))))))))))))))

(defun %s3-index-batch (cfg relative)
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
                                 (%s3-lazy-type-key cfg key (subseq bytes p2 after))
                                 (push (list key relative p2 after) out)
                                 (when (and (>= (length key) 4) (string= key "d:" :end1 2))
                                   (%s3-lazy-label-key cfg key (subseq bytes p2 after)))
                                 (setf cursor after))
                               (progn
                                 (push (list key relative nil nil :delete) out)
                                 (setf cursor p2)))))))))))
    (nreverse out)))

(defun %s3-load-lazy (cfg table)
  "Load ordinary objects and build a deterministic packed-segment index."
  (let ((token nil) (objects nil))
    (loop
      (let ((query (format nil "list-type=2&prefix=~a~:[~;~&continuation-token=~a~]"
                           (%s3-url-encode (s3-config-prefix cfg)) token
                           (and token (%s3-url-encode token)))))
        (multiple-value-bind (status headers body) (%s3-call cfg "GET" "" :query query)
          (declare (ignore headers))
          (unless (= status 200) (error "S3 LIST failed with HTTP ~d: ~a" status body))
          (dolist (object (%s3-xml-values body "Key"))
            (when (search (s3-config-prefix cfg) object)
              (push (subseq object (length (s3-config-prefix cfg))) objects)))
          (setf token (first (%s3-xml-values body "NextContinuationToken")))
          (unless token (return)))))
    (let ((ordinary nil) (segments nil) (tombstones nil))
      ;; S3 LIST order is not a mutation log.  Classify first and apply
      ;; immutable batches in their monotonic batch-id order.
      (dolist (relative objects)
        (let ((key (%s3-unhex-key relative)))
          (cond
            ((and (>= (length key) 7) (string= key "@batch:" :end1 7))
             (push relative segments))
            ((and (>= (length key) 11) (string= key "@tombstone:" :end1 11))
             (push key tombstones))
            (t (push (cons key relative) ordinary)))))
      (dolist (pair ordinary)
        (setf (gethash (car pair) table)
              (car (multiple-value-list
                    (codec-decode (%s3-get-cached cfg (cdr pair)))))))
      (dolist (relative (sort segments #'string<))
        (dolist (entry (%s3-index-batch cfg relative))
          (let ((key (first entry)) (delete-p (eq (fifth entry) :delete)))
            (when (gethash key (s3-config-lazy-index cfg))
              (setf (s3-config-summary-valid cfg) nil))
            (if delete-p
                (progn
                  (when (gethash key (s3-config-lazy-index cfg))
                    (%s3-lazy-count-key cfg key -1))
                  (remhash key (s3-config-lazy-index cfg))
                  (remhash key table)
                  (setf (s3-config-summary-valid cfg) nil))
                (progn
                  (unless (gethash key (s3-config-lazy-index cfg))
                    (%s3-lazy-count-key cfg key 1))
                  (remhash key table)
                  (setf (gethash key (s3-config-lazy-index cfg)) (cdr entry)))))))
      (dolist (marker tombstones)
        (let ((sep (position #\: marker :start 11)))
          (when sep
            (let ((key (%s3-unhex-key (subseq marker 11 sep))))
              (when (gethash key (s3-config-lazy-index cfg))
                (%s3-lazy-count-key cfg key -1))
              (remhash key (s3-config-lazy-index cfg))
              (remhash key table)
              (setf (s3-config-summary-valid cfg) nil)))))
      table)))

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
  (let ((entry (gethash key (s3-config-lazy-index cfg))))
    (when entry
      (let ((relative (first entry)) (start (second entry)) (end (third entry)))
        (when (and start end)
          (%s3-get-cached-range cfg relative start end))))))

(defun %s3-load (cfg table &optional decoder)
  (when (and (s3-config-lazy cfg) (null decoder))
    (return-from %s3-load (%s3-load-lazy cfg table)))
  (setf decoder (or decoder (lambda (bytes) (car (multiple-value-list (codec-decode bytes))))))
  "Load ordinary objects and then bulk mutation segments.
Segments are applied last so they can represent updates/deletes of older
individual objects while retaining deterministic restart semantics."
  (let ((token nil) (objects nil))
    (loop
      (let ((query (format nil "list-type=2&prefix=~a~:[~;~&continuation-token=~a~]"
                           (%s3-url-encode (s3-config-prefix cfg))
                           token (and token (%s3-url-encode token)))))
        (multiple-value-bind (status headers body) (%s3-call cfg "GET" "" :query query)
          (declare (ignore headers))
          (unless (= status 200) (error "S3 LIST failed with HTTP ~d: ~a" status body))
          (dolist (object (%s3-xml-values body "Key"))
            (when (search (s3-config-prefix cfg) object)
              (push (subseq object (length (s3-config-prefix cfg))) objects)))
          (setf token (first (%s3-xml-values body "NextContinuationToken")))
          (unless token (return)))))
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

(defun %s3-put-batch (cfg records)
  "Write a bulk mutation segment as one S3 object.
RECORDS contains (OP KEY VALUE), where OP is the string PUT or DELETE."
  (when records
    (let* ((id (format nil "@batch:~d-~d-~d" (get-universal-time)
                       (get-internal-real-time) (incf *s3-batch-sequence*)))
           (payload (codec-encode (list "scalaxy-s3-batch-v1" records))))
      (multiple-value-bind (status headers body)
          (%s3-call cfg "PUT" (%s3-hex-key id) :body payload)
        (declare (ignore headers))
        (unless (member status '(200 201 204))
          (error "S3 batch PUT failed with HTTP ~d: ~a" status body))))))

(defun %s3-put-raw (cfg key bytes)
  (multiple-value-bind (status headers body) (%s3-call cfg "PUT" (%s3-hex-key key) :body bytes)
    (declare (ignore headers))
    (unless (member status '(200 201 204))
      (error "S3 PUT failed with HTTP ~d: ~a" status body))
    (%s3-cache-invalidate cfg (%s3-hex-key key))))

(defun %s3-put (cfg key value)
  (multiple-value-bind (status headers body) (%s3-call cfg "PUT" (%s3-hex-key key) :body (codec-encode value))
    (declare (ignore headers))
    (unless (member status '(200 201 204))
      (error "S3 PUT failed with HTTP ~d: ~a" status body))
    (%s3-cache-invalidate cfg (%s3-hex-key key))))

(defun %s3-delete (cfg key)
  (let ((encoded (%s3-hex-key key)))
    (multiple-value-bind (status headers body) (%s3-call cfg "DELETE" encoded)
      (declare (ignore headers body))
      (unless (member status '(200 204)) (error "S3 DELETE failed with HTTP ~d" status)))
    (%s3-cache-invalidate cfg encoded)
    ;; A tombstone prevents an older packed import segment from resurrecting
    ;; this key when the store is reconstructed after restart.
    (let ((marker (format nil "@tombstone:~a:~d" encoded (get-universal-time))))
      (multiple-value-bind (status headers body)
          (%s3-call cfg "PUT" (%s3-hex-key marker)
                    :body (make-array 0 :element-type '(unsigned-byte 8)))
        (declare (ignore headers))
        (unless (member status '(200 201 204))
          (error "S3 tombstone PUT failed with HTTP ~d: ~a" status body))))))
