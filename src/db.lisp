;;;; db.lisp --- multiple databases per cluster
;;;;
;;;; Databases are logical namespaces implemented by physical key
;;;; prefixing: the physical key of (DB . KEY) is "d:<db>:<key>".
;;;; The wire protocol, the append-only log format and the consistent
;;;; hash ring are deliberately untouched: every layer below this one
;;;; sees only physical keys, so sharding, replication, durability and
;;;; prefix scanning work unchanged, per database.
;;;;
;;;; A database exists while its marker key "d:<name>:" is present.
;;;; The implicit database "default" always exists and is used whenever
;;;; no database is named.

(in-package #:scalaxy)

(defparameter +default-db+ "default"
  "Name of the implicit database used when none is named.")

(defun db-valid-name-p (name)
  "True when NAME is a valid database name ([A-Za-z0-9_-]{1,64})."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 64)
       (every (lambda (c) (or (alphanumericp c)
                              (char= c #\-)
                              (char= c #\_)))
              name)))

(defun db-prefix (db)
  "Physical key prefix of database DB (\"d:<db>:\")."
  (concatenate 'string "d:" db ":"))

(defun db-key (db key)
  "Physical key for logical KEY in database DB."
  (concatenate 'string (db-prefix db) key))

(defun db-parse-name (physical-key)
  "Database name a physical key belongs to, or NIL."
  (when (and (stringp physical-key)
             (>= (length physical-key) 2)
             (string= physical-key "d:" :end1 2))
    (let ((colon (position #\: physical-key :start 2)))
      (when colon
        (let ((name (subseq physical-key 2 colon)))
          (when (db-valid-name-p name)
            name))))))

(defun db-strip (db physical-key)
  "Logical key of PHYSICAL-KEY within DB, or NIL when it is not in DB."
  (let ((p (db-prefix db)))
    (when (and (>= (length physical-key) (length p))
               (string= physical-key p :end1 (length p)))
      (subseq physical-key (length p)))))

(defun db-list (pairs)
  "Database names with data-key counts from a scan of the \"d:\" prefix.
PAIRS is a list of (physical-key . value).  The marker key of each
database is not counted as data; \"default\" is always listed."
  (let ((names nil))
    (dolist (p pairs)
      (let ((name (db-parse-name (car p))))
        (when name
          (unless (member name names :test #'equal)
            (push name names)))))
    (unless (member +default-db+ names :test #'equal)
      (push +default-db+ names))
    (let ((counts (make-hash-table :test #'equal)))
      (dolist (p pairs)
        (let ((name (db-parse-name (car p))))
          (when (and name (plusp (length (db-strip name (car p)))))
            (incf (gethash name counts 0)))))
      (sort (mapcar (lambda (name) (cons name (or (gethash name counts) 0)))
                    names)
            #'string< :key #'car))))
