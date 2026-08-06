;;;; protocol.lisp --- binary wire protocol
;;;;
;;;; Message framing on the wire is: [4-byte big-endian body length][body]
;;;; where body is: [1-byte opcode][opcode-specific payload].
;;;;
;;;; The same record format is used for the append-only storage log,
;;;; so log records can be replayed and replicated verbatim.

(in-package #:scalaxy)

(defconstant +op-put+       1)
(defconstant +op-get+       2)
(defconstant +op-delete+    3)
(defconstant +op-scan+      4)
(defconstant +op-replicate+ 5)
(defconstant +op-ack+       6)
(defconstant +op-error+     7)
(defconstant +op-ping+      8)
(defconstant +op-pong+      9)
(defconstant +op-snapshot+  10)
(defconstant +op-response+  11)

(defconstant +status-ok+        0)
(defconstant +status-not-found+ 1)

(defun make-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun buf-write-u8 (buf x)
  (vector-push-extend (logand x #xFF) buf))

(defun buf-write-u32 (buf x)
  (loop for shift from 24 downto 0 by 8
        do (vector-push-extend (logand (ash x (- shift)) #xFF) buf)))

(defun buf-write-u64 (buf x)
  (loop for shift from 56 downto 0 by 8
        do (vector-push-extend (logand (ash x (- shift)) #xFF) buf)))

(defun buf-write-octets (buf octets)
  (buf-write-u32 buf (length octets))
  (loop for b across octets do (vector-push-extend b buf)))

(defun buf-write-string (buf string)
  (buf-write-octets buf (string-to-octets string)))

(defun read-u8 (v i)
  (values (aref v i) (1+ i)))

(defun read-u32 (v i)
  (values (+ (ash (aref v i) 24)
             (ash (aref v (+ i 1)) 16)
             (ash (aref v (+ i 2)) 8)
             (aref v (+ i 3)))
          (+ i 4)))

(defun read-u64 (v i)
  (values (+ (ash (aref v i) 56)
             (ash (aref v (+ i 1)) 48)
             (ash (aref v (+ i 2)) 40)
             (ash (aref v (+ i 3)) 32)
             (ash (aref v (+ i 4)) 24)
             (ash (aref v (+ i 5)) 16)
             (ash (aref v (+ i 6)) 8)
             (aref v (+ i 7)))
          (+ i 8)))

(defun read-octets (v i)
  (multiple-value-bind (len j) (read-u32 v i)
    (values (subseq v j (+ j len)) (+ j len))))

(defun read-string (v i)
  (multiple-value-bind (octets j) (read-octets v i)
    (values (octets-to-string octets) j)))

(defun encode-message (msg)
  "Encode message plist MSG (keys :op, :key, :value, :seq, :sub-op, :status,
:message, :pairs) into an octet vector."
  (let ((buf (make-buffer))
        (op (getf msg :op)))
    (buf-write-u8 buf op)
    (ecase op
      (#.+op-put+
       (buf-write-string buf (getf msg :key))
       (buf-write-octets buf (getf msg :value)))
      (#.+op-get+
       (buf-write-string buf (getf msg :key)))
      (#.+op-delete+
       (buf-write-string buf (getf msg :key)))
      (#.+op-scan+
       (buf-write-string buf (getf msg :prefix)))
      (#.+op-replicate+
       (buf-write-u64 buf (getf msg :seq))
       (buf-write-u8 buf (getf msg :sub-op))
       (if (= (getf msg :sub-op) #.+op-put+)
           (progn
             (buf-write-string buf (getf msg :key))
             (buf-write-octets buf (getf msg :value)))
           (buf-write-string buf (getf msg :key))))
      (#.+op-ack+
       (buf-write-u64 buf (or (getf msg :seq) 0))
       (buf-write-u8 buf (getf msg :status)))
      (#.+op-error+
       (buf-write-string buf (getf msg :message)))
      (#.+op-ping+)
      (#.+op-pong+)
      (#.+op-snapshot+
       (let ((pairs (getf msg :pairs)))
         (buf-write-u32 buf (length pairs))
         (dolist (p pairs)
           (buf-write-string buf (car p))
           (buf-write-octets buf (cdr p)))))
      (#.+op-response+
       (buf-write-u8 buf (getf msg :status))
       (buf-write-octets buf (or (getf msg :value) #()))
       (let ((pairs (getf msg :pairs)))
         (buf-write-u32 buf (length pairs))
         (dolist (p pairs)
           (buf-write-string buf (car p))
           (buf-write-octets buf (cdr p))))))
    buf))

(defun decode-message (v)
  "Decode an octet vector produced by ENCODE-MESSAGE into a message plist."
  (let ((op (aref v 0))
        (i 1))
    (case op
      (#.+op-put+
       (multiple-value-bind (key j) (read-string v i)
         (multiple-value-bind (value k) (read-octets v j)
           (list :op op :key key :value value))))
      (#.+op-get+
       (multiple-value-bind (key j) (read-string v i)
         (list :op op :key key)))
      (#.+op-delete+
       (multiple-value-bind (key j) (read-string v i)
         (list :op op :key key)))
      (#.+op-scan+
       (multiple-value-bind (prefix j) (read-string v i)
         (list :op op :prefix prefix)))
      (#.+op-replicate+
       (multiple-value-bind (seq j) (read-u64 v i)
         (multiple-value-bind (sub-op k) (read-u8 v j)
           (if (= sub-op #.+op-put+)
               (multiple-value-bind (key p) (read-string v k)
                 (multiple-value-bind (value q) (read-octets v p)
                   (list :op op :seq seq :sub-op sub-op :key key :value value)))
               (multiple-value-bind (key p) (read-string v k)
                 (list :op op :seq seq :sub-op sub-op :key key))))))
      (#.+op-ack+
       (multiple-value-bind (seq j) (read-u64 v i)
         (multiple-value-bind (status k) (read-u8 v j)
           (list :op op :seq seq :status status))))
      (#.+op-error+
       (multiple-value-bind (message j) (read-string v i)
         (list :op op :message message)))
      (#.+op-ping+ (list :op op))
      (#.+op-pong+ (list :op op))
      (#.+op-snapshot+
       (multiple-value-bind (n j) (read-u32 v i)
         (let ((pairs nil) (pos j))
           (loop repeat n
                 do (multiple-value-bind (key p) (read-string v pos)
                      (multiple-value-bind (value q) (read-octets v p)
                        (push (cons key value) pairs)
                        (setf pos q))))
           (list :op op :pairs (nreverse pairs)))))
      (#.+op-response+
       (multiple-value-bind (status j) (read-u8 v i)
         (multiple-value-bind (value k) (read-octets v j)
           (multiple-value-bind (n m) (read-u32 v k)
             (let ((pairs nil) (pos m))
               (loop repeat n
                     do (multiple-value-bind (key p) (read-string v pos)
                          (multiple-value-bind (val2 q) (read-octets v p)
                            (push (cons key val2) pairs)
                            (setf pos q))))
               (list :op op :status status :value value :pairs (nreverse pairs)))))))
      (t (list :op op)))))

(defun frame-message (msg)
  "Frame an encoded message with a 4-byte big-endian length prefix."
  (let* ((body (encode-message msg))
         (frame (make-buffer)))
    (buf-write-u32 frame (length body))
    (loop for b across body do (vector-push-extend b frame))
    frame))
