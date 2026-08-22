;;;; main.lisp --- standalone node entry point

(in-package #:scalaxy)

(defun env-get (name)
  "Read an environment variable."
  #+sbcl (sb-ext:posix-getenv name)
  #-sbcl nil)

(defun parse-peers (spec)
  "Parse a peer list 'id=host:data-port[,:http-port],id2=...' into
((id host data-port &optional http-port) ...)."
  (loop for part in (remove "" (split-sequence-on #\, (or spec "")) :test #'equal)
        for eq = (position #\= part)
        when (and part eq)
          collect (let* ((id (subseq part 0 eq))
                         (endpoint (subseq part (1+ eq)))
                         (parts (split-sequence-on #\: endpoint))
                         (host (first parts))
                         (data-port (and (second parts)
                                         (parse-integer (second parts)))))
                    (if (third parts)
                        (list id host data-port (parse-integer (third parts)))
                        (list id host data-port)))))

(defun parse-args (args)
  "Parse CLI flags.  Returns values for network, local-store, and optional S3 config."
  (let ((address "127.0.0.1:7200") (data-dir "./scalaxy-data") (id nil)
        (http-address "127.0.0.1:8080") (peers nil) (replicate-to nil)
        (web-dir "web/") (store-backend nil) (s3-endpoint nil) (s3-bucket nil)
        (s3-access-key nil) (s3-secret-key nil) (s3-region "us-east-1")
        (s3-prefix "scalaxy/") (encryption-key nil) (s3-lazy nil) (s3-streaming nil))
    (loop for (k v) on args by #'cddr
          do (cond ((string= k "--address") (setf address v))
                   ((string= k "--data-dir") (setf data-dir v))
                   ((string= k "--id") (setf id v))
                   ((string= k "--http-address") (setf http-address v))
                   ((string= k "--peers") (setf peers (parse-peers v)))
                   ((string= k "--replicate-to") (setf replicate-to (parse-peers v)))
                   ((string= k "--web-dir") (setf web-dir v))
                   ((string= k "--store-backend") (setf store-backend v))
                   ((string= k "--s3-endpoint") (setf s3-endpoint v))
                   ((string= k "--s3-bucket") (setf s3-bucket v))
                   ((string= k "--s3-access-key") (setf s3-access-key v))
                   ((string= k "--s3-secret-key") (setf s3-secret-key v))
                   ((string= k "--s3-region") (setf s3-region v))
                   ((string= k "--s3-prefix") (setf s3-prefix v))
                   ((string= k "--s3-encryption-key") (setf encryption-key v))
                   ((string= k "--s3-lazy")
                    (setf s3-lazy (not (null (member (string-downcase v) '("1" "true" "yes") :test #'string=)))))
                   ((string= k "--s3-streaming")
                    (setf s3-streaming (not (null (member (string-downcase v) '("1" "true" "yes") :test #'string=)))))))
    (values address data-dir id http-address peers replicate-to web-dir
            store-backend s3-endpoint s3-bucket s3-access-key s3-secret-key
            s3-region s3-prefix encryption-key s3-lazy s3-streaming)))

(defun env-args ()
  "Build a CLI argument list from SCALAXY_* environment variables."
  (let ((args nil))
    (flet ((add (flag value) (when value (push flag args) (push value args))))
      (add "--address" (env-get "SCALAXY_ADDRESS"))
      (add "--http-address" (env-get "SCALAXY_HTTP_ADDRESS"))
      (add "--data-dir" (env-get "SCALAXY_DATA_DIR"))
      (add "--id" (env-get "SCALAXY_NODE_ID"))
      (add "--peers" (env-get "SCALAXY_PEERS"))
      (add "--replicate-to" (env-get "SCALAXY_REPLICATE_TO"))
      (add "--web-dir" (env-get "SCALAXY_WEB_DIR"))
      (add "--store-backend" (env-get "SCALAXY_STORE_BACKEND"))
      (add "--s3-endpoint" (env-get "SCALAXY_S3_ENDPOINT"))
      (add "--s3-bucket" (env-get "SCALAXY_S3_BUCKET"))
      (add "--s3-access-key" (or (env-get "SCALAXY_S3_ACCESS_KEY") (env-get "AWS_ACCESS_KEY_ID")))
      (add "--s3-secret-key" (or (env-get "SCALAXY_S3_SECRET_KEY") (env-get "AWS_SECRET_ACCESS_KEY")))
      (add "--s3-region" (or (env-get "SCALAXY_S3_REGION") (env-get "AWS_REGION")))
      (add "--s3-prefix" (env-get "SCALAXY_S3_PREFIX"))
      (add "--s3-encryption-key" (env-get "SCALAXY_S3_ENCRYPTION_KEY"))
      (add "--s3-lazy" (env-get "SCALAXY_S3_LAZY"))
      (add "--s3-streaming" (env-get "SCALAXY_S3_STREAMING")))
    (nreverse args)))

(defun parse-host-port (address)
  (let ((pos (position #\: address)))
    (unless pos (error "invalid address ~a (expected HOST:PORT)" address))
    (values (subseq address 0 pos)
            (parse-integer (subseq address (1+ pos))))))

(defun make-node-store (&key node-id (data-dir "./scalaxy-data") store-backend
                              s3-endpoint s3-bucket s3-access-key s3-secret-key
                              (s3-region "us-east-1") (s3-prefix "scalaxy/") encryption-key lazy
                              streaming-mode peers)
  "Construct the node's durable store.  S3 prefixes are isolated per node."
  (if (or s3-endpoint (eq store-backend :s3)
          (and (stringp store-backend) (string-equal store-backend "s3")))
      (make-store :s3-endpoint s3-endpoint :s3-bucket s3-bucket
                  :s3-access-key s3-access-key :s3-secret-key s3-secret-key
                  :s3-region s3-region :encryption-key
                  (and encryption-key (string-to-octets encryption-key))
                  :s3-prefix (format nil "~a~a/" (string-right-trim "/" s3-prefix)
                                     (or node-id "node"))
                  :cache-path (merge-pathnames "s3-cache/"
                                               (uiop:ensure-directory-pathname data-dir))
                  :lazy lazy :streaming-mode streaming-mode
                  :owner-ring (when peers
                                (make-ring :nodes (mapcar #'first peers)))
                  :owner-id node-id)
      (make-store :path (merge-pathnames "scalaxy.log"
                                         (uiop:ensure-directory-pathname data-dir)))))

(defun start-node (&key (id nil) (address "127.0.0.1:7200")
                        (http-address "127.0.0.1:8080")
                        (data-dir "./scalaxy-data")
                        (peers nil) (replicate-to nil)
                        (web-dir "web/") (store-backend nil) (s3-endpoint nil)
                        (s3-bucket nil) (s3-access-key nil) (s3-secret-key nil)
                        (s3-region "us-east-1") (s3-prefix "scalaxy/") encryption-key lazy streaming-mode)
  "Start a Scalaxy node: durable store, TCP data server, and the web
console (HTTP).  When PEERS is given, the web console routes key operations
across the cluster and aggregates cluster status.  Returns
(values tcp-server http-server gateway-or-nil)."
    (when encryption-key
    (setf *s3-encryption-key*
          (if (stringp encryption-key)
              (string-to-octets encryption-key)
              encryption-key)))
  (multiple-value-bind (host port) (parse-host-port address)
    (multiple-value-bind (http-host http-port) (parse-host-port http-address)
      (let* ((node-id (or id
                          (or (env-get "SCALAXY_NODE_ID")
                              (env-get "HOSTNAME")
                              (format nil "node-~d" (get-universal-time)))))
             ;; A fixed log name keeps data discoverable across restarts
             ;; even if the node id or hostname changes.
             (node (make-node
                    :id node-id
                    :store (make-node-store :node-id node-id :data-dir data-dir
                                             :peers peers
                                             :store-backend store-backend
                                             :s3-endpoint s3-endpoint :s3-bucket s3-bucket
                                             :s3-access-key s3-access-key :s3-secret-key s3-secret-key
                                             :s3-region s3-region :s3-prefix s3-prefix
                                             :encryption-key encryption-key :lazy lazy)
                    :ring (when peers
                            (make-ring :nodes (mapcar #'first peers)))
                    :peers (loop for (pid phost pport) in peers
                                 collect (cons pid (list phost pport))))))
        ;; wire TCP replication followers
        (dolist (p replicate-to)
          (destructuring-bind (fid fhost fport) p
            (node-add-follower node fid
                               (lambda (msg) (tcp-request fhost fport msg)))))
        (let ((tcp-server (tcp-serve node :host host :port port))
              (gateway (when peers (make-gateway :peers peers :http-port http-port))))
          (let ((http-server
                  (http-serve
                   (make-web-handler :node node :gateway gateway :web-dir web-dir
                                     :address address :http-address http-address)
                   :host http-host :port http-port)))
            (format t "~&Scalaxy node ~a~%  data:  ~a:~d~%  http:  ~a:~d (web console)~%  data-dir: ~a~%  peers:  ~d~%"
                    node-id host (server-port tcp-server)
                    http-host (http-server-port http-server)
                    data-dir (length peers))
            (finish-output)
            ;; Retry failed follower replications periodically so a
            ;; follower that was down catches up after it returns.
            #+sbcl
            (when replicate-to
              (sb-thread:make-thread
               (lambda ()
                 (loop (sleep 5)
                       (ignore-errors (node-retry-replication node))))
               :name "scalaxy-outbox-retry"))
            (values tcp-server http-server gateway)))))))

(defun main (&rest argv)
  "Entry point for the standalone node.  ARGV is the CLI tail; when empty,
configuration is read from SCALAXY_* environment variables."
  (let ((args (or argv (env-args))))
    (multiple-value-bind (address data-dir id http-address peers replicate-to web-dir
                          store-backend s3-endpoint s3-bucket s3-access-key s3-secret-key
                          s3-region s3-prefix encryption-key lazy s3-streaming)
        (parse-args args)
      (start-node :id id :address address :http-address http-address
                  :data-dir data-dir :peers peers :replicate-to replicate-to
                  :web-dir web-dir :store-backend store-backend :s3-endpoint s3-endpoint
                  :s3-bucket s3-bucket :s3-access-key s3-access-key :s3-secret-key s3-secret-key
                  :s3-region s3-region :s3-prefix s3-prefix :lazy lazy :streaming-mode s3-streaming)
      (loop (sleep 3600)))))
