;;;; web.lisp --- web console: dashboard UI, JSON API, health endpoint
;;;;
;;;; Serves the Scalaxy web console (static assets from web/) and a small
;;;; JSON API backed either by the local node or, when a gateway is
;;;; configured, by the whole cluster (ring routing over TCP + HTTP status
;;;; aggregation).

(in-package #:scalaxy)

(defvar *web-asset-cache* (make-hash-table :test #'equal))

(defparameter +version+ "1.6.7")

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

(defun text-response (body &key (status 200) (content-type "text/plain; charset=utf-8"))
  (list :status status
        :headers (list (cons "Content-Type" content-type))
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

(defun node-status-plist (node &key (address "unknown") (http-address "unknown"))
  (list (cons "id" (node-id node))
        (cons "address" address)
        (cons "http" http-address)
        (cons "keys" (store-count (node-store node)))
        (cons "uptime" (max 0 (- (get-universal-time) (node-started-at node))))
        (cons "replicas" (length (node-followers node)))
        (cons "version" +version+)
        (cons "status" "ok")))

;;; ------------------------------------------------------------------
;;; command console

(defun run-command (node gateway command)
  "Execute a console command; returns (values output-text ok)."
  (let* ((parts (remove "" (split-sequence-on #\Space (string-trim " " command)) :test #'equal))
         (op (and parts (string-downcase (first parts)))))
    (cond
      ((null parts) (values "usage: put <key> <value> | get <key> | delete <key> | scan <prefix> [limit]" t))
      ((string= op "put")
       (when (< (length parts) 3) (values "put requires a key and a value" nil))
       (let* ((key (second parts))
              (value (string-to-octets (format nil "~{~a~^ ~}" (cddr parts))))
              (reply (if gateway (gateway-put gateway key value)
                         (node-dispatch node (list :op #.+op-put+ :key key :value value)))))
         (values (format nil "ok: stored ~a bytes under \"~a\"" (length value) key)
                 (and reply (eql (getf reply :status) #.+status-ok+)))))
      ((string= op "get")
       (when (< (length parts) 2) (values "get requires a key" nil))
       (let* ((key (second parts))
              (value (if gateway (gateway-get gateway key)
                         (node-get node key))))
         (if value
             (values (format nil "~a~%size: ~d bytes~%hex: ~a"
                             (octets-to-string value) (length value) (hex-digest value))
                     t)
             (values (format nil "not found: ~a" key) t))))
      ((string= op "delete")
       (when (< (length parts) 2) (values "delete requires a key" nil))
       (let* ((key (second parts))
              (reply (if gateway (gateway-delete gateway key)
                         (node-delete node key))))
         (values (format nil "deleted: ~a" key) (if (listp reply) t reply))))
      ((string= op "scan")
       (let* ((prefix (if (> (length parts) 1) (second parts) ""))
              (limit (if (> (length parts) 2) (parse-integer (third parts) :junk-allowed t) 100))
              (pairs (if gateway (gateway-scan gateway prefix :limit limit)
                         (node-scan node prefix))))
         (if pairs
             (values (format nil "~d key~:p matching \"~a\":~%~{~a~^~%~}"
                             (length pairs) prefix
                             (mapcar (lambda (p) (format nil "~a  (~d bytes)" (car p) (length (cdr p))))
                                     (subseq pairs 0 (min limit (length pairs)))))
                     t)
             (values (format nil "no keys match \"~a\"" prefix) t))))
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
         (segments (remove "" (split-sequence-on #\/ path) :test #'equal)))
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
             (text-response asset :content-type (mime-type name))
             (json-response (list (cons "error" "asset not found")) :status 404))))
      ;; node-local status (used for cluster aggregation)
      ((and (string= path "/api/node-status") (string= method "GET"))
       (json-response (node-status-plist node :address address :http-address http-address)))
      ;; cluster status
      ((and (string= path "/api/status") (string= method "GET"))
       (%api-status node gateway address http-address))
      ;; keys collection
      ((and (string= path "/api/keys") (string= method "GET"))
       (%api-keys node gateway request))
      ((and (string= path "/api/keys") (string= method "POST"))
       (json-response (list (cons "error" "use PUT /api/keys/<key>")) :status 405))
      ;; key item
      ((and (equal (first segments) "api") (equal (second segments) "keys") (= (length segments) 3))
       (let ((key (http-url-decode (third segments))))
         (cond
           ((string= method "GET") (%api-get-key node gateway key))
           ((string= method "PUT") (%api-put-key node gateway key request))
           ((string= method "DELETE") (%api-delete-key node gateway key))
           (t (json-response (list (cons "error" "method not allowed")) :status 405)))))
      ;; console
      ((and (string= path "/api/query") (string= method "POST"))
       (%api-query node gateway request))
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

(defun %api-keys (node gateway request)
  (let* ((query (getf request :query))
         (prefix (or (cdr (assoc "prefix" query :test #'equal)) ""))
         (limit (parse-integer (or (cdr (assoc "limit" query :test #'equal)) "200") :junk-allowed t))
         (offset (parse-integer (or (cdr (assoc "offset" query :test #'equal)) "0") :junk-allowed t))
         (pairs (if gateway (gateway-scan gateway prefix)
                    (node-scan node prefix)))
         (total (length pairs))
         (page (subseq pairs offset (min (+ offset limit) total))))
    (json-response
     (list (cons "keys"
                 (mapcar (lambda (p)
                           (list (cons "key" (car p))
                                 (cons "size" (length (cdr p)))
                                 (cons "preview" (preview (cdr p)))))
                         page))
           (cons "total" total)
           (cons "limit" limit)
           (cons "offset" offset)))))

(defun %api-get-key (node gateway key)
  (let ((value (if gateway (gateway-get gateway key) (node-get node key))))
    (if value
        (json-response (cons (cons "key" key) (value-info value)))
        (json-response (list (cons "error" "not found")) :status 404))))

(defun %api-put-key (node gateway key request)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (value-str (or (gethash "value" data) ""))
                   (value (string-to-octets value-str)))
              (let ((reply (if gateway (gateway-put gateway key value)
                               (node-dispatch node (list :op #.+op-put+ :key key :value value)))))
                (if (and reply (eql (getf reply :status) #.+status-ok+))
                    (json-response (list (cons "ok" t) (cons "key" key) (cons "size" (length value))))
                    (json-response (list (cons "error" "write failed")) :status 500))))
          (error (e) (json-response (list (cons "error" (princ-to-string e))) :status 400))))))

(defun %api-delete-key (node gateway key)
  (if gateway
      (let ((reply (gateway-delete gateway key)))
        (json-response (list (cons "ok" (and reply (eql (getf reply :status) #.+status-ok+))))))
      (json-response (list (cons "ok" (node-delete node key))))))

(defun %api-query (node gateway request)
  (let ((body (getf request :body)))
    (if (null body)
        (json-response (list (cons "error" "request body required")) :status 400)
        (handler-case
            (let* ((data (json-decode body))
                   (command (or (gethash "command" data) "")))
              (multiple-value-bind (output ok) (run-command node gateway command)
                (json-response (list (cons "ok" ok) (cons "output" output)))))
          (error (e) (json-response (list (cons "error" (princ-to-string e))) :status 400))))))
