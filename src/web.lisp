;;;; web.lisp --- web console: dashboard UI, JSON API, health endpoint
;;;;
;;;; Serves the Scalaxy web console (static assets from web/) and a small
;;;; JSON API backed either by the local node or, when a gateway is
;;;; configured, by the whole cluster (ring routing over TCP + HTTP status
;;;; aggregation).

(in-package #:scalaxy)

(defvar *web-asset-cache* (make-hash-table :test #'equal))

(defparameter +version+ "1.8.0")

(defun web-read-asset (web-dir rel)
  "Read an asset from WEB-DIR, cached in memory."
  (or (gethash rel *web-asset-cache*)
      (let ((path (merge-pathnames rel web-dir)))
        (when (probe-file path)
          (let ((content
                  (with-open-file (in path :direction :input :external-format :utf-8)
                    (let ((s (make-string (file-length in))))
                      (read-sequence s in)
                      s))))
            (setf (gethash rel *web-asset-cache*) content)
            content)))))

(defun mime-type (name)
  (cond ((string-equal name "css" :start1 (max 0 (- (length name) 3))) "text/css; charset=utf-8")
        ((string-equal name "js" :start1 (max 0 (- (length name) 2))) "application/javascript; charset=utf-8")
        ((string-equal name "svg" :start1 (max 0 (- (length name) 3))) "image/svg+xml")
        ((string-equal name "html" :start1 (max 0 (- (length name) 4))) "text/html; charset=utf-8")
        ((string-equal name "png" :start1 (max 0 (- (length name) 3))) "image/png")
        (t "application/octet-stream")))

(defun json-response (obj &key (status 200) (headers nil))
  (list :status status
        :headers (cons (cons "Content-Type" "application/json; charset=utf-8") headers)
        :body (json-encode obj)))

(defun text-response (body &key (status 200) (content-type "text/plain; charset=utf-8")
                              (headers nil))
  (list :status status
        :headers (append (list (cons "Content-Type" content-type)) headers)
        :body body))

(defun html-response (body &key (status 200))
  (text-response body :status status :content-type "text/html; charset=utf-8"))

;;; ------------------------------------------------------------------
;;; value helpers for the API

(defun value-info (value)
  (let ((size (length value)))
    (list (cons "size" size)
          (cons "utf8" (octets-to-string value))
          (cons "hex" (hex-digest value)))))

(defun preview (value &optional (max 64))
  (let ((s (octets-to-string value)))
    (if (> (length s) max)
        (concatenate 'string (subseq s 0 max) "...")
        s)))

;;; ------------------------------------------------------------------
;;; node status

(defvar *node-graph-metrics-cache* (make-hash-table :test #'equal))
(defun %node-graph-metrics (node db)
  (let ((key (list node db)))
    (or (gethash key *node-graph-metrics-cache*)
        (setf (gethash key *node-graph-metrics-cache*)
              (multiple-value-list (graph-store-counts (node-store node) :db db))))))
(defun invalidate-node-graph-metrics (node)
  (maphash (lambda (key value) (declare (ignore value))
             (when (eq (first key) node) (remhash key *node-graph-metrics-cache*)))
           *node-graph-metrics-cache*))

(defun node-status-plist (node &key (address "unknown") (http-address "unknown") (db +default-db+))
  (multiple-value-bind (graph-nodes graph-rels graph-labels)
      (ignore-errors (apply #'values (%node-graph-metrics node db)))
    (list (cons "id" (node-id node))
        (cons "address" address)
        (cons "http" http-address)
        (cons "keys" (store-count (node-store node)))
        (cons "graphNodes" (or graph-nodes 0))
        (cons "graphRelationships" (or graph-rels 0))
        (cons "s3SummaryValid"
              (let ((backend (store-backend (node-store node))))
                (and backend (typep backend 's3-config)
                     (s3-config-lazy backend)
                     (s3-config-summary-valid backend))))
        (cons "graphRelationshipTypeCounts"
              (let ((backend (store-backend (node-store node))))
                (if (and backend (typep backend 's3-config)
                         (s3-config-lazy backend))
                    (let ((types (gethash db (s3-config-lazy-type-counts backend)))
                          (pairs nil))
                      (when types (maphash (lambda (k v) (push (cons k v) pairs)) types))
                      pairs)
                    nil)))
        (cons "graphLabels" (if graph-labels (hash-table-count graph-labels) 0))
        (cons "graphLabelCounts"
              (if graph-labels
                  (let ((pairs nil))
                    (maphash (lambda (label count)
                               (push (cons label (or (and (numberp count) count) 1)) pairs))
                             graph-labels)
                    pairs)
                  nil))
        (cons "uptime" (max 0 (- (get-universal-time) (node-started-at node))))
        (cons "replicas" (length (node-followers node)))
        (cons "version" +version+)
        (cons "status" "ok"))))

;;; ------------------------------------------------------------------
;;; command console

(defun run-command (node gateway command &key (db +default-db+))
  "Execute a console command in database DB; returns (values output ok).
The USE command returns the new database name as its output."
  (let* ((parts (remove "" (split-sequence-on #\Space (string-trim " " command)) :test #'equal))
         (op (and parts (string-downcase (first parts)))))
    (cond
      ((null parts)
       (values (format nil "usage: put <key> <value> | get <key> | delete <key> | scan <prefix> [limit]~%cypher <query> | database: use <db> | databases (db: ~a)" db) t))
      ((string= op "put")
       (when (< (length parts) 3) (values "put requires a key and a value" nil))
       (let* ((key (second parts))
              (value (string-to-octets (format nil "~{~a~^ ~}" (cddr parts))))
              (reply (if gateway (gateway-put gateway (db-key db key) value)
                         (node-dispatch node (list :op #.+op-put+ :key (db-key db key) :value value)))))
         (values (format nil "ok: stored ~a bytes under \"~a\" (db: ~a)" (length value) key db)
                 (and reply (eql (getf reply :status) #.+status-ok+)))))
      ((string= op "get")
       (when (< (length parts) 2) (values "get requires a key" nil))
       (let* ((key (second parts))
              (value (if gateway (gateway-get gateway (db-key db key))
                         (node-get node (db-key db key)))))
         (if value
             (values (format nil "~a~%size: ~d bytes~%hex: ~a"
                             (octets-to-string value) (length value) (hex-digest value))
                     t)
             (values (format nil "not found: ~a (db: ~a)" key db) t))))
      ((string= op "delete")
       (when (< (length parts) 2) (values "delete requires a key" nil))
       (let* ((key (second parts))
              (reply (if gateway (gateway-delete gateway (db-key db key))
                         (node-delete node (db-key db key)))))
         (values (format nil "deleted: ~a (db: ~a)" key db) (if (listp reply) t reply))))
      ((string= op "scan")
       (let* ((prefix (if (> (length parts) 1) (second parts) ""))
              (limit (if (> (length parts) 2) (parse-integer (third parts) :junk-allowed t) 100))
              (pairs (if gateway (gateway-scan gateway (db-key db prefix) :limit limit)
                         (node-scan node (db-key db prefix)))))
         (if pairs
             (values (format nil "~d key~:p matching \"~a\" (db: ~a):~%~{~a~^~%~}"
                             (length pairs) prefix db
                             (mapcar (lambda (p) (format nil "~a  (~d bytes)" (db-strip db (car p)) (length (cdr p))))
                                     (subseq pairs 0 (min limit (length pairs)))))
                     t)
             (values (format nil "no keys match \"~a\" (db: ~a)" prefix db) t))))
      ((string= op "use")
       (let ((name (and (> (length parts) 1) (second parts))))
         (cond ((null name) (values "usage: use <database>" nil))
               ((not (db-valid-name-p name)) (values (format nil "invalid database name: ~a" name) nil))
               (t (values name t)))))
      ((string= op "databases")
       (let ((dbs (if gateway (gateway-list-databases gateway)
                      (mapcar #'car (db-list (node-scan node "d:"))))))
         (values (format nil "databases:~%~{  ~a~^~%~}" dbs) t)))
      ((string= op "cypher")
       (when (<= (length parts) 1)
         (return-from run-command (values "usage: cypher <query> (db: " db ")" nil)))
       (let* ((query (string-left-trim
                      '(#\Space)
                      (subseq command (1+ (position #\Space command)))))
              (rows (handler-case
                        (if gateway
                            (gateway-cypher gateway query :db db)
                            (cypher-query query
                                          (make-local-graph (node-store node) :db db)))
                      (error (e)
                        (return-from run-command
                          (values (format nil "cypher error: ~a" e) nil))))))
         (if (null rows)
             (values "(no rows)" t)
             (let* ((columns (mapcar #'car (first rows)))
                    (lines (loop for row in rows
                                 collect (format nil "~{~a~^ | ~}"
                                                 (mapcar (lambda (c)
                                                           (cypher-print-value
                                                            (cdr (assoc c row))))
                                                         columns)))))
               (values (format nil "~d row~:p~%~{~a~^~%~}" (length rows) lines) t)))))
      (t (values (format nil "unknown command: ~a" op) nil)))))

;;; ------------------------------------------------------------------
;;; HTTP handler

(defun make-web-handler (&key node (gateway nil) (web-dir "web/") (address "unknown")
                              (http-address "unknown"))
  (lambda (request)
    (%web-dispatch request node gateway web-dir address http-address)))

(defun %web-dispatch (request node gateway web-dir address http-address)
  (let* ((method (getf request :method))
         (path (getf request :path))
         (segments (remove "" (split-sequence-on #\/ path) :test #'equal))
         (db (or (cdr (assoc "db" (getf request :query) :test #'equal))
                 +default-db+)))
    (cond
      ;; health
      ((and (string= path "/healthz") (member method '("GET" "HEAD") :test #'string=))
       (if (string= method "HEAD")
           (list :status 200 :headers nil :body "")
           (json-response (list (cons "status" "ok")))))
      ;; index
      ((and (string= path "/") (string= method "GET"))
       (let ((html (web-read-asset web-dir "index.html")))
         (if html (html-response html)
             (html-response "<h1>Scalaxy Console</h1><p>web/index.html not found</p>" :status 500))))
      ;; assets
      ((and (string= method "GET") (equal segments '("assets")))
       (json-response (list (cons "error" "missing asset name")) :status 400))
      ((and (string= method "GET") (equal (first segments) "assets") (= (length segments) 2))
       (let* ((name (second segments))
              (asset (web-read-asset web-dir (format nil "assets/~a" name))))
         (if asset
             (text-response asset :content-type (mime-type name)
                            :headers (list (cons "Cache-Control" "no-cache")))
             (json-response (list (cons "error" "asset not found")) :status 404))))
      ;; node-local status (used for cluster aggregation)
      ((and (string= path "/api/node-status") (string= method "GET"))
       (json-response (node-status-plist node :address address :http-address http-address :db db)))
      ;; databases (multiple databases per cluster)
      ((and (string= path "/api/databases") (string= method "GET"))
       (%api-list-databases node gateway))
      ((and (string= path "/api/databases") (string= method "POST"))
       (%api-create-database node gateway request))
      ((and (equal (first segments) "api") (equal (second segments) "databases")
             (= (length segments) 3) (string= method "DELETE"))
       (%api-drop-database node gateway (http-url-decode (third segments))))
      ;; cluster status
      ((and (string= path "/api/status") (string= method "GET"))
       (%api-status node gateway address http-address))
      ;; keys collection
      ((and (string= path "/api/keys") (string= method "GET"))
       (%api-keys node gateway request db))
      ((and (string= path "/api/keys") (string= method "POST"))
       (json-response (list (cons "error" "use PUT /api/keys/<key>")) :status 405))
      ;; packed, owner-routed graph import
      ((and (string= path "/api/bulk-keys") (string= method "POST"))
       (%api-bulk-keys node gateway request db))
      ;; key item
      ((and (equal (first segments) "api") (equal (second segments) "keys") (= (length segments) 3))
       (let ((key (http-url-decode (third segments))))
         (cond
           ((string= method "GET") (%api-get-key node gateway db key))
           ((string= method "PUT") (%api-put-key node gateway db key request))
           ((string= method "DELETE") (%api-delete-key node gateway db key))
           (t (json-response (list (cons "error" "method not allowed")) :status 405)))))
      ;; console
      ((and (string= path "/api/query") (string= method "POST"))
       (%api-query node gateway request db))
      ;; cypher
      ((and (string= path "/api/cypher") (string= method "POST"))
       (%api-cypher node gateway request db))
      ;; graphql
      ((and (string= path "/api/graphql") (string= method "POST"))
       (%api-graphql node request db))
      ;; fallback
      (t (json-response (list (cons "error" "not found") (cons "path" path)) :status 404)))))

(defun %api-status (node gateway address http-address)
  (if gateway
      (let ((status (gateway-status gateway)))
        (json-response
         (list (cons "node" (node-status-plist node :address address :http-address http-address))
               (cons "nodes" (getf status :nodes))
               (cons "cluster"
                     (list (cons "nodes" (length (getf status :nodes)))
                           (cons "keys" (getf status :total-keys))
                           (cons "replicas" 1)
                           (cons "status"
                                 (if (every (lambda (n) (string= (gethash "status" n) "ok"))
                                            (getf status :nodes))
                                     "healthy" "degraded"))))
               (cons "ring" (%ring-shares gateway)))))
      (json-response
       (list (cons "node" (node-status-plist node :address address :http-address http-address))
             (cons "nodes" (list (node-status-plist node :address address :http-address http-address)))
             (cons "cluster"
                   (list (cons "nodes" 1)
                         (cons "keys" (store-count (node-store node)))
                         (cons "replicas" (length (node-followers node)))
                         (cons "status" "healthy")))
             (cons "ring" (list (list (cons "id" (node-id node)) (cons "share" 1.0))))))))

(defun %ring-shares (gateway)
  "Estimate each node's share of the keyspace from the ring."
  (let* ((ids (mapcar #'car (gateway-peers gateway)))
         (vnodes (ring-vnodes (gateway-ring gateway)))
         (counts (make-hash-table :test #'equal))
         (total 0))
    (dolist (id ids) (setf (gethash id counts) 0))
    (loop for i below (length vnodes)
          for id = (cdr (aref vnodes i))
          do (incf (gethash id counts))
             (incf total))
    (loop for id in ids
          collect (list (cons "id" id)
                        (cons "share" (if (zerop total) 0.0
                                          (float (/ (gethash id counts) total))))))))

(defun %api-list-databases (node gateway)
  (let* ((pairs (if gateway (gateway-scan gateway "d:")
                    (node-scan node "d:")))
         (dbs (db-list pairs)))
    (json-response
     (list (cons "databases"
                 (mapcar (lambda (p)
                           (list (cons "name" (car p))
                                 (cons "keys" (cdr p))))
                         dbs))))))

(defun %api-create-database (node gateway request)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (name (or (gethash "name" data) "")))
              (unless (db-valid-name-p name)
                (return-from %api-create-database
                  (json-response (list (cons "error"
                                             (format nil "invalid database name ~s" name)))
                                 :status 400)))
              (let ((reply (if gateway (gateway-create-database gateway name)
                               (node-dispatch node (list :op #.+op-put+
                                                         :key (db-key name "")
                                                         :value #())))))
                (if (and reply (eql (getf reply :status) #.+status-ok+))
                    (json-response (list (cons "ok" t) (cons "name" name)))
                    (json-response (list (cons "error" "create failed")) :status 500))))
          (error (e) (json-response (list (cons "error" (princ-to-string e))) :status 400))))))

(defun %api-drop-database (node gateway name)
  (cond
    ((equal name +default-db+)
     (json-response (list (cons "error" "cannot drop the implicit database \"default\""))
                    :status 400))
    (gateway
     (gateway-drop-database gateway name)
     (json-response (list (cons "ok" t) (cons "name" name))))
    (t
     (dolist (p (node-scan node (db-key name "")))
       (node-delete node (car p)))
     (json-response (list (cons "ok" t) (cons "name" name))))))

(defun %api-bulk-keys (node gateway request db)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (items (or (gethash "records" data) nil))
                   (pairs
                     (mapcar
                      (lambda (item)
                        (let ((key (gethash "key" item))
                              (hex (gethash "value" item)))
                          (unless (and (stringp key) (stringp hex)
                                       (evenp (length hex)))
                            (error "invalid bulk record"))
                          (cons (db-key db key) (%hex-octets hex))))
                      items))
                   (reply (if gateway
                              (gateway-bulk-put gateway pairs)
                              (node-dispatch node (list :op #.+op-bulk-put+
                                                        :pairs pairs)))))
              (json-response (list (cons "ok" t)
                                   (cons "count" (or (getf reply :count)
                                                     (getf reply :seq)
                                                     (length pairs))))))
          (error (e)
            (json-response (list (cons "error" (princ-to-string e))) :status 400))))))

(defun %api-keys (node gateway request db)
  (let* ((query (getf request :query))
         (prefix (or (cdr (assoc "prefix" query :test #'equal)) ""))
         (limit (parse-integer (or (cdr (assoc "limit" query :test #'equal)) "200") :junk-allowed t))
         (offset (parse-integer (or (cdr (assoc "offset" query :test #'equal)) "0") :junk-allowed t))
         (pairs (remove-if (lambda (p) (let ((k (db-strip db (car p))))
                                            (or (null k) (zerop (length k)))))
                            (if gateway (gateway-scan gateway (db-key db prefix))
                                (node-scan node (db-key db prefix)))))
         (total (length pairs))
         (page (subseq pairs offset (min (+ offset limit) total))))
    (json-response
     (list (cons "keys"
                 (mapcar (lambda (p)
                           (list (cons "key" (db-strip db (car p)))
                                 (cons "size" (length (cdr p)))
                                 (cons "preview" (preview (cdr p)))))
                         page))
           (cons "total" total)
           (cons "limit" limit)
           (cons "offset" offset)))))

(defun %api-get-key (node gateway db key)
  (let ((value (if gateway (gateway-get gateway (db-key db key))
                   (node-get node (db-key db key)))))
    (if value
        (json-response (list (cons "key" key) (cons "db" db)
                             (cons "size" (length value))
                             (cons "utf8" (octets-to-string value))
                             (cons "hex" (hex-digest value))))
        (json-response (list (cons "error" "not found")) :status 404))))

(defun %api-put-key (node gateway db key request)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (value-str (or (gethash "value" data) ""))
                   (value (string-to-octets value-str)))
              (let ((reply (if gateway (gateway-put gateway (db-key db key) value)
                               (node-dispatch node (list :op #.+op-put+
                                                         :key (db-key db key) :value value)))))
                (if (and reply (eql (getf reply :status) #.+status-ok+))
                    (json-response (list (cons "ok" t) (cons "key" key) (cons "db" db)
                                         (cons "size" (length value))))
                    (json-response (list (cons "error" "write failed")) :status 500))))
          (error (e) (json-response (list (cons "error" (princ-to-string e))) :status 400))))))

(defun %api-delete-key (node gateway db key)
  (if gateway
      (let ((reply (gateway-delete gateway (db-key db key))))
        (json-response (list (cons "ok" (and reply (eql (getf reply :status) #.+status-ok+))))))
      (json-response (list (cons "ok" (node-delete node (db-key db key)))))))

(defun %api-cypher (node gateway request db)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (query (or (gethash "query" data) ""))
                   (params (gethash "params" data))
                   (db (or (gethash "db" data) db)))
              (let ((rows (if gateway
                              (gateway-cypher gateway query :db db :params params)
                              (cypher-query query
                                            (make-local-graph (node-store node) :db db)
                                            :params params))))
                (list :status 200
                      :headers (list (cons "Content-Type" "application/json; charset=utf-8"))
                      :body (cypher-result->json rows))))
          (scalaxy:cypher-error (e)
            (json-response (list (cons "error" (format nil "~a" e))
                                 (cons "kind" (cypher-error-kind e)))
                           :status 400))
          (error (e)
            (json-response (list (cons "error" (princ-to-string e))) :status 500))))))

(defun %api-graphql (node request db)
  "POST /api/graphql -- execute a GraphQL query against the local graph
database.  Body: {\"query\": ..., \"variables\": {...}, \"db\": ...}.
Returns a standard GraphQL document (data + errors) with an
extensions.graph block carrying the materialized node/edge set."
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (query (or (gethash "query" data) ""))
                   (vars (gethash "variables" data))
                   (db (or (gethash "db" data) db)))
              (if (string= query "")
                  (json-response (list (cons "error" "query required")) :status 400)
                  (json-response
                   (graphql-execute query
                                    (make-local-graph (node-store node) :db db)
                                    :variables vars))))
          (error (e)
            (json-response (list (cons "error" (princ-to-string e))) :status 500))))))

(defun %api-query (node gateway request db)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (command (or (gethash "command" data) "")))
              (multiple-value-bind (output ok) (run-command node gateway command :db db)
                (json-response (list (cons "ok" ok) (cons "output" output)
                                     (cons "db" db)))))
          (error (e) (json-response (list (cons "error" (princ-to-string e))) :status 400))))))
