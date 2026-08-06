;;;; util.lisp --- small portable helpers

(in-package #:scalaxy)

(defun string-to-octets (string)
  "Encode STRING as an octet vector (ASCII-compatible byte mapping)."
  (let ((v (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for i below (length string)
          do (setf (aref v i) (char-code (char string i))))
    v))

(defun octets-to-string (octets)
  "Decode an octet vector produced by STRING-TO-OCTETS back to a string."
  (let ((s (make-string (length octets))))
    (loop for i below (length octets)
          do (setf (char s i) (code-char (aref octets i))))
    s))

(defun hex-digest (octets)
  "Render OCTETS as a lowercase hexadecimal string."
  (with-output-to-string (out)
    (loop for b across octets
          do (format out "~2,'0x" b))))

(defun fnv1a-64 (data &key (seed #x84222325cbf29ce4))
  "FNV-1a 64-bit hash of the octet vector DATA."
  (loop with hash = seed
        for b across data
        do (setf hash (logand (* (logxor hash b) #x100000001b3)
                              #xFFFFFFFFFFFFFFFF))
        finally (return hash)))

(defun hash-string (string)
  "FNV-1a 64-bit hash of a string key."
  (fnv1a-64 (string-to-octets string)))
