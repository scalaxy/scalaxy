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

(defun fnv1a-64 (data &key (seed #xcbf29ce484222325))
  "FNV-1a 64-bit hash of the octet vector DATA.
The seed is the standard FNV-1a offset basis (0xCBF29CE484222325)."
  (loop with hash = seed
        for b across data
        do (setf hash (logand (* (logxor hash b) #x100000001b3)
                              #xFFFFFFFFFFFFFFFF))
        finally (return hash)))

(defun splitmix64 (x)
  "SplitMix64 finalizer: bijective avalanche that scatters correlated
64-bit inputs (e.g. FNV-1a of strings that differ in one byte)."
  (let ((z (logand (+ x #x9e3779b97f4a7c15) #xFFFFFFFFFFFFFFFF)))
    (setf z (logand (logxor z (ash z -30)) #xFFFFFFFFFFFFFFFF))
    (setf z (logand (* z #xbf58476d1ce4e5b9) #xFFFFFFFFFFFFFFFF))
    (setf z (logand (logxor z (ash z -27)) #xFFFFFFFFFFFFFFFF))
    (setf z (logand (* z #x94d049bb133111eb) #xFFFFFFFFFFFFFFFF))
    (logand (logxor z (ash z -31)) #xFFFFFFFFFFFFFFFF)))

(defun hash-string (string)
  "Uniform 64-bit hash of a string key (FNV-1a + SplitMix64 finalizer)."
  (splitmix64 (fnv1a-64 (string-to-octets string))))
