;;;; tests/run-tests.lisp --- Scalaxy test suite

(in-package #:scalaxy-tests)

(defvar *checks* 0)
(defvar *failures* 0)

(defmacro deftest (name &body body)
  `(progn
     (format t "~&== ~a ==~%" ,(string name))
     ,@body))

(defun check (result &optional (msg ""))
  (incf *checks*)
  (unless result
    (incf *failures*)
    (format t "  FAIL: ~a~%" msg)))

(defun check-equal (a b &optional (msg ""))
  (check (equal a b) (format nil "~a (expected ~s, got ~s)" msg b a)))

(defun deep-equal (a b)
  "Equality that compares octet vectors element-wise (EQUAL does not)."
  (cond ((and (vectorp a) (vectorp b) (not (stringp a)) (not (stringp b)))
         (and (= (length a) (length b))
              (loop for i below (length a) always (deep-equal (aref a i) (aref b i)))))
        ((and (consp a) (consp b))
         (and (deep-equal (car a) (car b)) (deep-equal (cdr a) (cdr b))))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (equal a b))))

;;; ------------------------------------------------------------------
;;; consistent hashing

(defun test-consistent-hash ()
  (deftest consistent-hash
    (let* ((nodes '("a" "b" "c"))
           (ring (make-ring :nodes nodes :vnodes-per-node 128))
           (keys (loop for i below 3000
                       collect (format nil "key-~d" i)))
           (owners (make-hash-table :test #'equal)))
      ;; every key maps to a member of the cluster, deterministically
      (dolist (k keys)
        (let ((owner (ring-lookup ring k)))
          (check (member owner nodes :test #'equal) "owner is a cluster member")
          (check (equal owner (ring-lookup ring k)) "lookup is deterministic")
          (incf (gethash owner owners 0))))
      ;; reasonably balanced
      (let ((min-share (loop for n in nodes minimize (gethash n owners 0))))
        (check (> min-share (/ (length keys) 20)) "no node starved"))
      ;; adding a node moves only a minority of keys
      (let* ((ring2 (ring-add ring "d"))
             (moved (loop for k in keys
                          count (not (equal (ring-lookup ring k)
                                            (ring-lookup ring2 k))))))
        (check (< moved (* 0.40 (length keys)))
               (format nil "adding a node moved ~d/~d keys" moved (length keys))))
      ;; removing a node never moves keys owned by other nodes
      (let* ((ring3 (ring-remove ring "b")))
        (dolist (k keys)
          (let ((before (ring-lookup ring k)))
            (unless (equal before "b")
              (check (equal before (ring-lookup ring3 k))
                     "removing a node leaves other owners untouched"))))))))

;;; ------------------------------------------------------------------
;;; storage

(defun test-storage ()
  (deftest storage
    (let ((store (make-store)))
      (store-put store "alpha" (string-to-octets "1"))
      (store-put store "alpine" (string-to-octets "2"))
      (store-put store "beta" (string-to-octets "3"))
      (check-equal (octets-to-string (store-get store "alpha")) "1" "get alpha")
      (check-equal (octets-to-string (store-get store "alpine")) "2" "get alpine")
      (check (null (store-get store "missing")) "get missing")
      (check-equal (length (store-scan store "al")) 2 "scan prefix 'al'")
      (check-equal (length (store-scan store "")) 3 "scan all")
      (store-delete store "alpha")
      (check (null (store-get store "alpha")) "delete alpha")
      (check-equal (store-count store) 2 "count after delete"))
    ;; persistence through the append-only log
    (let* ((dir (format nil "/tmp/scalaxy-test-~d" (get-universal-time)))
           (path (format nil "~a/store.log" dir)))
      (ensure-directories-exist path)
      (unwind-protect
           (progn
             (let ((s (make-store :path path)))
               (store-put s "k1" (string-to-octets "v1"))
               (store-put s "k2" (string-to-octets "v2"))
               (store-delete s "k1"))
             (let ((s2 (make-store :path path)))
               (check (null (store-get s2 "k1")) "replay: deleted key stays deleted")
               (check-equal (octets-to-string (store-get s2 "k2")) "v2" "replay: surviving key")))
        (delete-file path)))))

;;; ------------------------------------------------------------------
;;; protocol round-trips

(defun test-protocol ()
  (deftest protocol
    (let ((blob (make-array 256 :element-type '(unsigned-byte 8))))
      (loop for i below 256 do (setf (aref blob i) i))
      (dolist (msg (list (list :op #.+op-put+ :key "k" :value blob)
                         (list :op #.+op-get+ :key "k")
                         (list :op #.+op-delete+ :key "k")
                         (list :op #.+op-scan+ :prefix "pre")
                         (list :op #.+op-replicate+ :seq 42 :sub-op #.+op-put+
                               :key "rk" :value (string-to-octets "rv"))
                         (list :op #.+op-replicate+ :seq 43 :sub-op #.+op-delete+
                               :key "rk")
                         (list :op #.+op-ack+ :seq 42 :status #.+status-ok+)
                         (list :op #.+op-error+ :message "boom")
                         (list :op #.+op-ping+)
                         (list :op #.+op-pong+)
                         (list :op #.+op-snapshot+
                               :pairs (list (cons "a" (string-to-octets "1"))
                                            (cons "b" (string-to-octets "2"))))
                         (list :op #.+op-response+ :status #.+status-ok+
                               :value (string-to-octets "v")
                               :pairs (list (cons "p1" (string-to-octets "x"))))))
        (let ((decoded (decode-message (encode-message msg))))
          (check (equal (getf decoded :op) (getf msg :op)) "opcode preserved")
          (check (equal (getf decoded :key) (getf msg :key)) "key preserved")
          (check (deep-equal (getf decoded :value) (getf msg :value)) "value preserved")
          (check (equal (getf decoded :seq) (getf msg :seq)) "seq preserved")
          (check (equal (getf decoded :sub-op) (getf msg :sub-op)) "sub-op preserved")
          (check (equal (getf decoded :status) (getf msg :status)) "status preserved")
          (check (equal (getf decoded :message) (getf msg :message)) "message preserved")
          (check (equal (getf decoded :prefix) (getf msg :prefix)) "prefix preserved")
          (check (deep-equal (getf decoded :pairs) (getf msg :pairs)) "pairs preserved"))))))

;;; ------------------------------------------------------------------
;;; node + replication

(defun test-replication ()
  (deftest replication
    (let ((leader (make-node :id "leader"))
          (follower (make-node :id "follower")))
      (node-add-follower leader "follower"
                         (lambda (msg) (node-dispatch follower msg)))
      (node-put leader "shared" (string-to-octets "42"))
      (check-equal (octets-to-string (node-get follower "shared")) "42"
                   "follower received replicated write")
      (node-put leader "shared2" (string-to-octets "43"))
      (check-equal (octets-to-string (node-get follower "shared2")) "43"
                   "second replicated write")
      (node-delete leader "shared")
      (check (null (node-get follower "shared")) "replicated delete")
      (check (plusp (replicator-seq (node-replicator leader))) "op log advanced"))))

;;; ------------------------------------------------------------------
;;; cluster

(defun test-cluster ()
  (deftest cluster
    (let ((cluster (make-cluster :ids '("n1" "n2" "n3") :replicas 1))
          (keys (loop for i below 200 collect (format nil "user:~d" i))))
      ;; writes route by consistent hash and replicate once
      (dolist (k keys)
        (cluster-put cluster k (string-to-octets (format nil "v-~a" k))))
      (dolist (k keys)
        (check (equal (octets-to-string (cluster-get cluster k))
                      (format nil "v-~a" k))
               (format nil "cluster read-back ~a" k)))
      ;; each key exists on exactly its primary; with 1 replica total
      ;; copies = 2 * number of keys
      (let ((total (loop for id in '("n1" "n2" "n3")
                         sum (store-count (node-store
                                           (gethash id (cluster-nodes cluster)))))))
        (check-equal total (* 2 (length keys)) "one synchronous replica per key"))
      ;; scans aggregate across nodes
      (check-equal (length (cluster-scan cluster "user:")) (length keys)
                   "cluster scan")
      ;; deletes
      (dolist (k (subseq keys 0 50))
        (cluster-delete cluster k))
      (check (null (cluster-get cluster (first keys))) "deleted key is gone")
      ;; node churn: add a node, keep reading and writing
      (cluster-add-node cluster "n4")
      (dolist (k (loop for i from 200 below 250 collect (format nil "user:~d" i)))
        (cluster-put cluster k (string-to-octets "new")))
      (dolist (k (loop for i from 200 below 250 collect (format nil "user:~d" i)))
        (check (equal (octets-to-string (cluster-get cluster k)) "new")
               (format nil "read-back after churn ~a" k)))
      (cluster-remove-node cluster "n2")
      (dolist (k (loop for i from 200 below 250 collect (format nil "user:~d" i)))
        (check (equal (octets-to-string (cluster-get cluster k)) "new")
               (format nil "read-back after removal ~a" k))))))

;;; ------------------------------------------------------------------
;;; TCP server / client over real sockets

#+sbcl
(defun test-tcp ()
  (deftest tcp
    (let ((node (make-node :id "tcp-node"))
          (server nil)
          (port nil))
      (unwind-protect
           (progn
             (setf server (tcp-serve node :host "127.0.0.1" :port 0))
             (setf port (server-port server))
             (check (plusp port) "server bound to an ephemeral port")
             ;; wait until the accept loop is up
             (let ((ready nil))
               (dotimes (i 100)
                 (when (ignore-errors
                         (let ((r (tcp-request "127.0.0.1" port (list :op #.+op-ping+))))
                           (and r (eql (getf r :op) #.+op-pong+))))
                   (setf ready t)
                   (return))
                 (sleep 0.02))
               (check ready "ping/pong over TCP"))
             (let ((r (tcp-request "127.0.0.1" port
                                   (list :op #.+op-put+ :key "tcp-key"
                                         :value (string-to-octets "tcp-value")))))
               (check (and r (eql (getf r :status) #.+status-ok+)) "TCP put"))
             (let ((r (tcp-request "127.0.0.1" port
                                   (list :op #.+op-get+ :key "tcp-key"))))
               (check (deep-equal (getf r :value) (string-to-octets "tcp-value"))
                      "TCP get round-trip"))
             (let ((r (tcp-request "127.0.0.1" port
                                   (list :op #.+op-scan+ :prefix "tcp-"))))
               (check-equal (length (getf r :pairs)) 1 "TCP scan"))
             (let ((r (tcp-request "127.0.0.1" port
                                   (list :op #.+op-delete+ :key "tcp-key"))))
               (check (and r (eql (getf r :status) #.+status-ok+)) "TCP delete")))
        (when server (tcp-stop server))))))

#-sbcl
(defun test-tcp () (format t "~&== tcp == (skipped: requires SBCL)~%"))

;;; ------------------------------------------------------------------


;;; ------------------------------------------------------------------
;;; hostname resolution (needed for container/K8s peer discovery)

(defun test-resolve-host ()
  (deftest resolve-host
    (check (ip-string-p "127.0.0.1") "ip-string-p true for dotted quad")
    (check (not (ip-string-p "scalaxy-1")) "ip-string-p false for hostname")
    #+sbcl
    (progn
      (check (equalp (resolve-host "127.0.0.1") #(127 0 0 1)) "resolve-host ip string")
      (let ((addr (resolve-host "localhost")))
        (check (and (vectorp addr) (= (length addr) 4)) "resolve-host hostname -> 4 octets")))))

;;; ------------------------------------------------------------------
;;; JSON

(defun test-json ()
  (deftest json
    (let* ((obj (let ((h (make-hash-table :test #'equal)))
                  (setf (gethash "name" h) "scalaxy"
                        (gethash "count" h) 42
                        (gethash "ok" h) t
                        (gethash "nothing" h) nil
                        (gethash "tags" h) '("a" "b" "c")
                        (gethash "nested" h) (let ((h2 (make-hash-table :test #'equal)))
                                               (setf (gethash "x" h2) 1.5)
                                               h2))
                  h))
           (decoded (json-decode (json-encode obj))))
      (check (string= (gethash "name" decoded) "scalaxy") "json string")
      (check (= (gethash "count" decoded) 42) "json integer")
      (check (eq (gethash "ok" decoded) t) "json true")
      (check (null (gethash "nothing" decoded)) "json null")
      (check (equal (gethash "tags" decoded) '("a" "b" "c")) "json array")
      (check (= (gethash "x" (gethash "nested" decoded)) 1.5) "json nested object"))
    (let ((s (json-decode (json-encode (format nil "a~c~c~c~c~c~c~c~c"
                                               #\" #\\ #\c #\Newline #\d #\Tab #\e (code-char #x1234))))))
      (check (string= s (format nil "a~c~c~c~c~c~c~c~c"
                                #\" #\\ #\c #\Newline #\d #\Tab #\e (code-char #x1234)))
             "json escaping round-trip"))
    (check (equal (json-decode "[1,2,3]") '(1 2 3)) "json literal array")
    (check (null (json-decode "null")) "json null literal")
    (let* ((decoded (json-decode "{\"a\":{\"b\":[true,false]}}"))
           (a (gethash "a" decoded))
           (b (and a (gethash "b" a))))
      (check (hash-table-p a) "json nested object")
      (check (equal b '(t nil)) "json nested literals"))))

;;; ------------------------------------------------------------------
;;; HTTP server / client

(defun test-http ()
  (deftest http
    (let ((server (http-serve
                   (lambda (req)
                     (cond ((string= (getf req :path) "/echo")
                            (json-response (list (cons "method" (getf req :method))
                                                 (cons "body" (getf req :body)))))
                           ((string= (getf req :path) "/decode")
                            (json-response (list (cons "q" (getf req :query)))))
                           (t (json-response (list (cons "error" "nf")) :status 404))))
                   :port 0)))
      (unwind-protect
           (let ((port (http-server-port server)))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "POST" "/echo" :body "{\"a\":1}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 200) "http status 200")
                 (check (string= (gethash "method" d) "POST") "http method echo")
                 (check (string= (gethash "body" d) "{\"a\":1}") "http body echo")))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/decode?x=1&y=hello%20world")
               (declare (ignore hdrs))
               (let ((q (gethash "q" (json-decode body))))
                 (check (string= (gethash "y" q) "hello world") "query y decode")
                 (check (string= (gethash "x" q) "1") "query x")))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/nope")
               (declare (ignore hdrs body))
               (check (= status 404) "http 404"))
             (check (string= (http-url-decode "a%20b+c") "a b c") "url decode"))
        (http-stop server)))))

;;; ------------------------------------------------------------------
;;; web API (node + HTTP console)

(defun test-web-api ()
  (deftest web-api
    (let* ((node (make-node :id "web"))
           (tcp-server (tcp-serve node :port 0))
           (http-server (http-serve
                         (make-web-handler :node node :web-dir "web/"
                                           :address "127.0.0.1:7200"
                                           :http-address "127.0.0.1:8080")
                         :port 0)))
      (unwind-protect
           (let ((port (http-server-port http-server)))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/healthz")
               (declare (ignore hdrs))
               (check (= status 200) "healthz status")
               (check (string= (gethash "status" (json-decode body)) "ok") "healthz body"))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/")
               (declare (ignore hdrs))
               (check (and (= status 200) (search "Scalaxy Console" body)) "index page"))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "PUT" "/api/keys/web-key"
                                                                   :body "{\"value\":\"web-value\"}")
               (declare (ignore hdrs))
               (check (= status 200) "api put status")
               (check (gethash "ok" (json-decode body)) "api put ok"))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/api/keys/web-key")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 200) "api get status")
                 (check (string= (gethash "utf8" d) "web-value") "api get utf8")
                 (check (string= (gethash "hex" d) (hex-digest (string-to-octets "web-value"))) "api get hex")))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "GET" "/api/keys")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= (gethash "total" d) 1) "api keys total")
                 (check (string= (gethash "key" (first (gethash "keys" d))) "web-key") "api keys list")))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "POST" "/api/query"
                                                                   :body "{\"command\":\"get web-key\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (gethash "ok" d) "query ok")
                 (check (search "web-value" (gethash "output" d)) "query output")))
             (multiple-value-bind (status hdrs body) (http-request "127.0.0.1" port "DELETE" "/api/keys/web-key")
               (declare (ignore hdrs body))
               (check (= status 200) "api delete status")))
        (http-stop http-server)
        (tcp-stop tcp-server)))))

;;; ------------------------------------------------------------------
;;; gateway: ring routing over real TCP

(defun test-gateway ()
  (deftest gateway
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "g~d" i))))
           (servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (ports (mapcar #'server-port servers))
           (peers (loop for i below 3
                        collect (list (format nil "g~d" i) "127.0.0.1" (nth i ports))))
           (gw (make-gateway :peers peers)))
      (unwind-protect
           (progn
             (dolist (k '("aaa" "bbb" "ccc" "ddd" "eee"))
               (gateway-put gw k (string-to-octets (format nil "v-~a" k))))
             (dolist (k '("aaa" "bbb" "ccc" "ddd" "eee"))
               (check (deep-equal (gateway-get gw k) (string-to-octets (format nil "v-~a" k)))
                      (format nil "gateway get ~a" k)))
             (check (= (length (gateway-scan gw "")) 5) "gateway scan total")
             (check (= (length (gateway-scan gw "c")) 1) "gateway scan prefix")
             (gateway-delete gw "aaa")
             (check (null (gateway-get gw "aaa")) "gateway delete")
             (check (= (reduce #'+ (mapcar (lambda (n) (store-count (node-store n))) nodes)) 4)
                    "keys distributed across nodes"))
        (dolist (s servers) (tcp-stop s))))))

(defun test-gateway-status ()
  (deftest gateway-status
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "s~d" i))))
           (tcp-servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (http-servers (loop for n in nodes
                               collect (http-serve (make-web-handler :node n :web-dir "web/")
                                                   :port 0)))
           (peers (loop for i below 3
                        collect (list (format nil "s~d" i) "127.0.0.1"
                                      (server-port (nth i tcp-servers))
                                      (http-server-port (nth i http-servers)))))
           (gw (make-gateway :peers peers)))
      (unwind-protect
           (progn
             (gateway-put gw "k1" (string-to-octets "1"))
             (gateway-put gw "k2" (string-to-octets "2"))
             (gateway-put gw "k3" (string-to-octets "3"))
             (let ((status (gateway-status gw)))
               (check (= (length (getf status :nodes)) 3) "status aggregates 3 nodes")
               (check (= (getf status :total-keys) 3) "status total keys")
               (check (every (lambda (n) (string= (gethash "status" n) "ok"))
                             (getf status :nodes))
                      "all peers report ok")))
        (dolist (s http-servers) (http-stop s))
        (dolist (s tcp-servers) (tcp-stop s))))))

(defun test-gateway-failover ()
  (deftest gateway-failover
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "f~d" i))))
           (servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (ports (mapcar #'server-port servers))
           (peers (loop for i below 3
                        collect (list (format nil "f~d" i) "127.0.0.1" (nth i ports))))
           (gw (make-gateway :peers peers)))
      ;; wire one synchronous replica per key: f0->f1, f1->f2, f2->f0
      (loop for i below 3
            for n in nodes
            do (node-add-follower n (format nil "f~d" (mod (1+ i) 3))
                                  (lambda (msg) (node-dispatch (nth (mod (1+ i) 3) nodes) msg))))
      (unwind-protect
           (progn
             (dolist (k (loop for i below 30 collect (format nil "fail:~d" i)))
               (gateway-put gw k (string-to-octets (format nil "v-~a" k))))
             ;; kill node f2
             (tcp-stop (nth 2 servers))
             (let ((missing 0))
               (dolist (k (loop for i below 30 collect (format nil "fail:~d" i)))
                 (unless (deep-equal (gateway-get gw k) (string-to-octets (format nil "v-~a" k)))
                   (incf missing)))
               (check (zerop missing)
                      (format nil "all keys readable after one node failure (~d missing)" missing))))
        (dolist (s servers) (ignore-errors (tcp-stop s)))))))

;;; ------------------------------------------------------------------

(defun run-all-tests ()
  (setf *checks* 0 *failures* 0)
  (test-consistent-hash)
  (test-storage)
  (test-protocol)
  (test-replication)
  (test-cluster)
  (test-tcp)
  (test-resolve-host)
  (test-json)
  (test-http)
  (test-web-api)
  (test-gateway)
  (test-gateway-status)
  (test-gateway-failover)
  (format t "~&~%Ran ~d checks, ~d failure~:p.~%" *checks* *failures*)
  (if (zerop *failures*) 0 1))

