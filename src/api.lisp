;;;; api.lisp --- high-level client API over TCP

(in-package #:scalaxy)

(defstruct (client (:constructor make-client (host port)))
  host
  port)

(defun connect (&key (host "127.0.0.1") (port 7200))
  (make-client host port))

(defun %value (value)
  (etypecase value
    (string (string-to-octets value))
    ((vector (unsigned-byte 8)) value)))

(defun put (client key value)
  "Store KEY -> VALUE on the cluster.  VALUE may be a string or an octet
vector.  Returns T on success."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-put+ :key key :value (%value value)))))
    (and reply (eql (getf reply :status) #.+status-ok+))))

(defun get (client key)
  "Fetch KEY.  Returns the value (octet vector) or NIL when absent."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-get+ :key key))))
    (when (and reply (eql (getf reply :status) #.+status-ok+))
      (getf reply :value))))

(defun delete (client key)
  "Delete KEY.  Returns T if the key was present."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-delete+ :key key))))
    (and reply (eql (getf reply :status) #.+status-ok+))))

(defun scan (client prefix)
  "Return all (key . value) pairs whose key starts with PREFIX."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-scan+ :prefix prefix))))
    (when (and reply (eql (getf reply :status) #.+status-ok+))
      (getf reply :pairs))))
