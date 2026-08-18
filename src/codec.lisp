;;;; codec.lisp --- compact binary codec for Cypher values
;;;;
;;;; Cypher values are represented in memory as follows (the contract
;;;; for every module above this one):
;;;;
;;;;   NULL    -> the keyword :CYPHER-NULL   (never CL NIL)
;;;;   TRUE    -> T
;;;;   FALSE   -> the keyword :CYPHER-FALSE  (never CL NIL)
;;;;   INTEGER -> CL integer
;;;;   FLOAT   -> CL float
;;;;   STRING  -> CL string
;;;;   LIST    -> CL list; the EMPTY list is the sentinel #()
;;;;   BYTES   -> (simple-array (unsigned-byte 8)) octet vector
;;;;   MAP     -> (:cypher-map (string-key . value) ...)
;;;;
;;;; CL NIL is reserved for "absence" in internal APIs and must never be
;;;; passed to the encoder; the encoder signals an error if it is, so
;;;; representation bugs are caught at the boundary instead of corrupting
;;;; records.  This is what separates Cypher's NULL/FALSE/[] from Lisp's
;;;; overloaded NIL (plan section 4.6).
;;;;
;;;; Wire layout: one tag byte, then a payload.
;;;;   0 null  | 1 true | 2 false | 3 int (i64 BE) | 4 float (f64 BE)
;;;;   5 string (u32 len + utf8)  | 6 bytes (u32 len + raw)
;;;;   7 list (u32 count + items) | 8 map (u32 count + (string,value)*)

(in-package #:scalaxy)

(defconstant +tag-null+   0)
(defconstant +tag-true+   1)
(defconstant +tag-false+  2)
(defconstant +tag-int+    3)
(defconstant +tag-float+  4)
(defconstant +tag-string+ 5)
(defconstant +tag-bytes+  6)
(defconstant +tag-list+   7)
(defconstant +tag-map+    8)
(defconstant +tag-temporal+ 9)

(defun cypher-null-p (v) (eq v :cypher-null))
(defun cypher-false-p (v) (eq v :cypher-false))
(defun cypher-true-p (v) (eq v t))

(defun cypher-list-p (v)
  "True when V is a Cypher list: a proper CL list, or #() (the empty
list sentinel).  Dotted pairs, maps, graph entities and temporal values
are not lists."
  (or (and (consp v) (not (eq (car v) :cypher-map))
           (not (%entity-plist-p v))
           (not (%temporal-p v))
           (null (cdr (last v))))
      (cypher-empty-list-p v)))

(defun cypher-empty-list-p (v)
  (and (vectorp v) (zerop (length v))
       (not (stringp v))
       (not (typep v '(vector (unsigned-byte 8))))))

(defun cypher-list (v)
  "Coerce a CL list (NIL for empty) to the internal list representation."
  (if v v #()))

(defun cypher-list-elements (v)
  "Elements of a Cypher list (NIL for the empty list)."
  (if (cypher-empty-list-p v) nil v))

(defun cypher-map (pairs)
  "Wrap PAIRS (alist of (string-key . value)) as a Cypher MAP value.
The wrapper removes the list-of-pairs ambiguity: a Cypher map is
(:CYPHER-MAP . pairs), never a bare alist."
  (cons :cypher-map pairs))

(defun cypher-map-p (v)
  (and (consp v) (eq (car v) :cypher-map)))

(defun cypher-map-pairs (v)
  (if (cypher-map-p v) (cdr v) v))

;;; ------------------------------------------------------------------
;;; IEEE-754 double helpers (SBCL fast path, portable fallback)

(defun double-float-bits (x)
  "Raw IEEE-754 bits of X as an unsigned 64-bit integer."
  #+sbcl (sb-kernel:double-float-bits x)
  #-sbcl
  (cond ((zerop x)
         (if (= (float-sign x) -1.0) #x8000000000000000 0))
        (t
         (multiple-value-bind (m e s) (integer-decode-float x)
           (let ((biased (+ e 52 1023)))
             (unless (<= 1 biased 2046)
               (error "codec: cannot encode out-of-range float ~a" x))
             (logior (if (minusp s) #x8000000000000000 0)
                     (ash biased 52)
                     (- m #x10000000000000)))))))

(defun bits-double-float (bits)
  "Double float whose raw IEEE-754 bits are BITS (unsigned u64)."
  #+sbcl (sb-kernel:%make-double-float (if (logbitp 63 bits)
                                           (- bits #x10000000000000000)
                                           bits))
  #-sbcl
  (let* ((sign (if (logbitp 63 bits) -1 1))
         (biased (logand (ash bits -52) #x7FF))
         (frac (logand bits #xFFFFFFFFFFFFF)))
    (cond ((= biased 0) (if (zerop frac) 0.0d0
                            (* sign (scale-float (float frac 1.0d0) -1074))))
          ((= biased #x7FF) (error "codec: cannot decode NaN/infinity"))
          (t (let ((value (* sign (scale-float (+ (float frac 1.0d0)
                                                  (float #x10000000000000 1.0d0))
                                               (- biased 1023 52)))))
               (coerce value 'double-float))))))

;;; ------------------------------------------------------------------
;;; encoding

(defun %codec-write (buf v)
  (cond
    ((cypher-null-p v) (buf-write-u8 buf +tag-null+))
    ((eq v t) (buf-write-u8 buf +tag-true+))
    ((cypher-false-p v) (buf-write-u8 buf +tag-false+))
    ((integerp v)
     (buf-write-u8 buf +tag-int+)
     (buf-write-u64 buf (logand v #xFFFFFFFFFFFFFFFF)))
    ((floatp v)
     (buf-write-u8 buf +tag-float+)
     (buf-write-u64 buf (double-float-bits (coerce v 'double-float))))
    ((stringp v)
     (buf-write-u8 buf +tag-string+)
     (buf-write-string buf v))
    ((typep v '(vector (unsigned-byte 8)))
     (buf-write-u8 buf +tag-bytes+)
     (buf-write-octets buf v))
    ((cypher-map-p v)
     (let ((pairs (cypher-map-pairs v)))
       (buf-write-u8 buf +tag-map+)
       (buf-write-u32 buf (length pairs))
       (dolist (p pairs)
         (buf-write-string buf (car p))
         (%codec-write buf (cdr p)))))
    ((cypher-list-p v)
     (buf-write-u8 buf +tag-list+)
     (let ((items (cypher-list-elements v)))
       (buf-write-u32 buf (length items))
       (dolist (x items) (%codec-write buf x))))
    ((%temporal-p v)
     (buf-write-u8 buf +tag-temporal+)
     (let ((ints (%temporal-encode-ints v)))
       (buf-write-u8 buf (length ints))
       (dolist (z ints)
         (buf-write-u64 buf (logand z #xFFFFFFFFFFFFFFFF)))))
    (t (error "codec: cannot encode ~s (CL NIL is not a Cypher value)" v))))

(defun codec-encode (value)
  "Encode a Cypher value as an octet vector."
  (let ((buf (make-buffer)))
    (%codec-write buf value)
    buf))

;;; ------------------------------------------------------------------
;;; decoding

(defun %codec-read (v i)
  "Decode one value from octet vector V at position I.
Returns (values value next-position)."
  (let ((tag (aref v i)))
    (case tag
      (#.+tag-null+ (values :cypher-null (1+ i)))
      (#.+tag-true+ (values t (1+ i)))
      (#.+tag-false+ (values :cypher-false (1+ i)))
      (#.+tag-int+
       (multiple-value-bind (u j) (read-u64 v (1+ i))
         (values (if (logbitp 63 u) (- u #x10000000000000000) u) j)))
      (#.+tag-float+
       (multiple-value-bind (u j) (read-u64 v (1+ i))
         (values (bits-double-float u) j)))
      (#.+tag-string+
       (multiple-value-bind (s j) (read-string v (1+ i))
         (values s j)))
      (#.+tag-bytes+
       (multiple-value-bind (b j) (read-octets v (1+ i))
         (values b j)))
      (#.+tag-list+
       (multiple-value-bind (n j) (read-u32 v (1+ i))
         (let ((items nil) (pos j))
           (loop repeat n
                 do (multiple-value-bind (x p) (%codec-read v pos)
                      (push x items)
                      (setf pos p)))
           (values (cypher-list (nreverse items)) pos))))
      (#.+tag-map+
       (multiple-value-bind (n j) (read-u32 v (1+ i))
         (let ((pairs nil) (pos j))
           (loop repeat n
                 do (multiple-value-bind (k p) (read-string v pos)
                      (multiple-value-bind (x q) (%codec-read v p)
                        (push (cons k x) pairs)
                        (setf pos q))))
           (values (cypher-map (nreverse pairs)) pos))))
      (#.+tag-temporal+
       (multiple-value-bind (n j) (read-u8 v (1+ i))
         (let ((ints nil) (pos j))
           (loop repeat n
                 do (multiple-value-bind (u q) (read-u64 v pos)
                      (push u ints)
                      (setf pos q)))
           (values (%temporal-from-encode-ints (nreverse ints)) pos))))
      (t (error "codec: unknown tag ~d at position ~d" tag i)))))

(defun codec-decode (octets &optional (start 0))
  "Decode a Cypher value from OCTETS at START.
Returns (values value next-position)."
  (%codec-read octets start))

;;; ------------------------------------------------------------------
;;; value equality (Cypher semantics, section 4.6 of the plan)

(defun cypher-value= (a b)
  "Equality of two in-memory Cypher values."
  (cond
    ((and (cypher-list-p a) (cypher-list-p b))
     (let ((as (cypher-list-elements a)) (bs (cypher-list-elements b)))
       (and (= (length as) (length bs))
            (every #'cypher-value= as bs))))
    ((and (cypher-map-p a) (cypher-map-p b))
     ;; maps: compare as key/value multisets (order-insensitive)
     (let ((as (cypher-map-pairs a)) (bs (cypher-map-pairs b)))
       (and (= (length as) (length bs))
            (every (lambda (p)
                     (let ((q (assoc (car p) bs :test #'equal)))
                       (and q (cypher-value= (cdr p) (cdr q)))))
                   as))))
    ((and (typep a '(vector (unsigned-byte 8)))
          (typep b '(vector (unsigned-byte 8)))
          (not (stringp a)) (not (stringp b)))
     (and (= (length a) (length b))
          (loop for i below (length a) always (= (aref a i) (aref b i)))))
    ((and (integerp a) (integerp b)) (= a b))
    ((and (floatp a) (floatp b)) (= a b))
    ((and (stringp a) (stringp b)) (string= a b))
    (t (equal a b))))
