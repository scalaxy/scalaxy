;;;; api.lisp --- high-level client API over TCP
;;;;
;;;; Every operation names a database (default: "default").  Databases
;;;; are logical namespaces: the client maps (DB . KEY) to a physical
;;;; key via DB-KEY, and scans are scoped to the database prefix, so a
;;;; client of any node sees exactly the same logical keyspace as every
;;;; other client regardless of sharding.

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

(defun %put (client key value)
  "Store physical KEY -> VALUE over TCP.  Returns T on success."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-put+ :key key :value value))))
    (and reply (eql (getf reply :status) #.+status-ok+))))

(defun %get (client key)
  "Fetch physical KEY.  Returns the value (octet vector) or NIL."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-get+ :key key))))
    (when (and reply (eql (getf reply :status) #.+status-ok+))
      (getf reply :value))))

(defun %delete (client key)
  "Delete physical KEY.  Returns T on success."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-delete+ :key key))))
    (and reply (eql (getf reply :status) #.+status-ok+))))

(defun %scan (client prefix)
  "Return all (physical-key . value) pairs whose key starts with PREFIX."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-scan+ :prefix prefix))))
    (when (and reply (eql (getf reply :status) #.+status-ok+))
      (getf reply :pairs))))

(defun put (client key value &key (db +default-db+))
  "Store KEY -> VALUE in database DB.  VALUE may be a string or an octet
vector.  Returns T on success."
  (%put client (db-key db key) (%value value)))

(defun get (client key &key (db +default-db+))
  "Fetch KEY from database DB.  Returns the value (octet vector) or NIL
when absent."
  (%get client (db-key db key)))

(defun delete (client key &key (db +default-db+))
  "Delete KEY from database DB.  Returns T on success."
  (%delete client (db-key db key)))

(defun scan (client prefix &key (db +default-db+))
  "Return all (key . value) pairs in database DB whose key starts with
PREFIX.  The database marker key (the empty logical key) is reserved
and never returned."
  (loop for (pkey . value) in (%scan client (db-key db prefix))
        for stripped = (db-strip db pkey)
        when (and stripped (plusp (length stripped)))
          collect (cons stripped value)))

(defun cypher (client query &key (db +default-db+) params)
  "Evaluate Cypher QUERY in database DB on the cluster (through the
node the client is connected to).  Returns the decoded JSON result
table (a hash-table with \"columns\", \"rows\", \"count\")."
  (let ((reply (tcp-request (client-host client) (client-port client)
                            (list :op #.+op-cypher+ :db db :query query
                                  :params (string-to-octets
                                           (if params (json-encode params) ""))))))
    (when (and reply (eql (getf reply :status) #.+status-ok+))
      (json-decode (octets-to-string (getf reply :value))))))

(defun create-database (client name)
  "Create database NAME in the cluster (writes its marker key).
Returns T on success."
  (unless (db-valid-name-p name)
    (error "Scalaxy: invalid database name ~s (expected [A-Za-z0-9_-]{1,64})" name))
  (%put client (db-key name "") #()))

(defun list-databases (client)
  "Return the names of all databases in the cluster, sorted."
  (mapcar #'car (db-list (%scan client "d:"))))

(defun drop-database (client name)
  "Delete database NAME and every key it contains.  The implicit
\"default\" database cannot be dropped.  Returns the remaining names."
  (when (equal name +default-db+)
    (error "Scalaxy: cannot drop the implicit database \"default\""))
  (dolist (p (%scan client (db-key name "")))
    (%delete client (car p)))
  (list-databases client))
