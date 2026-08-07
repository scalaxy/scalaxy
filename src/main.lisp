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
  "Parse CLI flags from ARGS.  Returns
(values address data-dir id http-address peers replicate-to web-dir)."
  (let ((address "127.0.0.1:7200")
        (data-dir "./scalaxy-data")
        (id nil)
        (http-address "127.0.0.1:8080")
        (peers nil)
        (replicate-to nil)
        (web-dir "web/"))
    (loop for (k v) on args by #'cddr
          do (cond ((string= k "--address") (setf address v))
                   ((string= k "--data-dir") (setf data-dir v))
                   ((string= k "--id") (setf id v))
                   ((string= k "--http-address") (setf http-address v))
                   ((string= k "--peers") (setf peers (parse-peers v)))
                   ((string= k "--replicate-to") (setf replicate-to (parse-peers v)))
                   ((string= k "--web-dir") (setf web-dir v))))
    (values address data-dir id http-address peers replicate-to web-dir)))

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
      (add "--web-dir" (env-get "SCALAXY_WEB_DIR")))
    (nreverse args)))

(defun parse-host-port (address)
  (let ((pos (position #\: address)))
    (unless pos (error "invalid address ~a (expected HOST:PORT)" address))
    (values (subseq address 0 pos)
            (parse-integer (subseq address (1+ pos))))))

(defun start-node (&key (id nil) (address "127.0.0.1:7200")
                        (http-address "127.0.0.1:8080")
                        (data-dir "./scalaxy-data")
                        (peers nil) (replicate-to nil)
                        (web-dir "web/"))
  "Start a Scalaxy node: durable store, TCP data server, and the web
console (HTTP).  When PEERS is given, the web console routes key operations
across the cluster and aggregates cluster status.  Returns
(values tcp-server http-server gateway-or-nil)."
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
                    :store (make-store
                            :path (merge-pathnames
                                   "scalaxy.log"
                                   (uiop:ensure-directory-pathname data-dir))))))
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
            (values tcp-server http-server gateway)))))))

(defun main (&rest argv)
  "Entry point for the standalone node.  ARGV is the CLI tail; when empty,
configuration is read from SCALAXY_* environment variables."
  (let ((args (or argv (env-args))))
    (multiple-value-bind (address data-dir id http-address peers replicate-to web-dir)
        (parse-args args)
      (start-node :id id :address address :http-address http-address
                  :data-dir data-dir :peers peers :replicate-to replicate-to
                  :web-dir web-dir)
      (loop (sleep 3600)))))
