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
;;; cypher front end: lexer + parser + printer

(defun %lex-kinds (s)
  (mapcar #'cytoken-kind (cypher-lex s)))

(defun test-cypher-lexer ()
  (deftest cypher-lexer
    (check (equal (%lex-kinds "MATCH (a) RETURN a")
                  '(:ident :punct :ident :punct :ident :ident :eof))
           "lexer basic kinds")
    (check (equal (mapcar #'cytoken-value (remove :eof (cypher-lex "x1 _y foo")
                                                  :key #'cytoken-kind))
                  '("x1" "_y" "foo"))
           "lexer ident values")
    (check (equal (cytoken-value (car (cypher-lex "'it\\'s'"))) "it's") "lexer escape")
    (check (equal (cytoken-value (car (cypher-lex "'\\u00e9'"))) "é") "lexer unicode")
    (check (equal (cytoken-value (car (cypher-lex "42"))) 42) "lexer int")
    (check (equal (cytoken-value (car (cypher-lex "0x2A"))) 42) "lexer hex")
    (check (equal (cytoken-value (car (cypher-lex "052"))) 42) "lexer octal")
    (check (equal (cytoken-value (car (cypher-lex "4.5"))) 4.5) "lexer float")
    (check (equal (cytoken-value (car (cypher-lex "1e3"))) 1000.0) "lexer exponent")
    (check (equal (cytoken-kind (car (cypher-lex "$name"))) :param) "lexer param")
    (check (equal (%lex-kinds (format nil "1 // comment~% 2"))
                  '(:int :int :eof))
           "lexer line comment")
    ;; openCypher block comments are not nested: the comment ends at the
    ;; first */, so "b */ 2" lexes as b, *, /, 2
    (check (equal (%lex-kinds "1 /* a /* nested? */ b */ 2")
                  '(:int :ident :punct :punct :int :eof))
           "lexer block comment")
    (check (equal (mapcar #'cytoken-value
                          (remove-if-not (lambda (tok) (eql (cytoken-kind tok) :punct))
                                         (cypher-lex "a-->b<--c--d")))
                  '("-->" "<--" "--"))
           "lexer arrows")
    (check (handler-case (progn (cypher-lex "0x") nil)
             (cypher-error (e) (string= (cypher-error-kind e) "InvalidNumberLiteral")))
           "hex without digits")
    (check (handler-case (progn (cypher-lex "9999999999999999999999") nil)
             (cypher-error (e) (string= (cypher-error-kind e) "IntegerOverflow")))
           "integer overflow")
    (check (handler-case (progn (cypher-lex "'unterminated") nil)
             (cypher-error (e) (string= (cypher-error-kind e) "UnexpectedSyntax")))
           "unterminated string")))

(defun test-cypher-parser ()
  (deftest cypher-parser
    ;; simple match/return
    (check (equal (cypher-parse "MATCH (n) RETURN n")
                  (list :query
                        (list :match
                              :pattern (list (list (list :node :var (ast-var "n") :labels nil :props nil)))
                              :where nil)
                        (list :return :items (list (list :item :expr (ast-var "n") :as nil))
                              :distinct nil :order nil)))
           "parse MATCH (n) RETURN n")
    ;; labels, rel, where, projection aliases, order/skip/limit
    (let ((q (cypher-parse "MATCH (a:Person)-[r:KNOWS]->(b) WHERE b.age > 30 RETURN a.name AS n, count(*) AS c ORDER BY n DESC SKIP 1 LIMIT 10")))
      (let ((match (second q)) (ret (third q))
            (skip (fourth q)) (limit (fifth q)))
        (check (eq (car match) :match) "match clause head")
        (check (equal (getf (cdr match) :where)
                      (list :bin :> (list :prop :expr (ast-var "b") :prop "age")
                            (list :lit 30)))
               "where parsed")
        (check (equal (second (getf (cdr ret) :items))
                      (list :item :expr (list :count-*) :as (ast-var "c")))
               "count(*) parsed")
        (check (equal (getf (cdr ret) :order)
                      (list (list :spec :expr (ast-var "n") :desc t)))
               "order by desc")
        (check (equal (getf (cdr skip) :expr) (list :lit 1)) "skip")
        (check (equal (getf (cdr limit) :expr) (list :lit 10)) "limit")))
    ;; pattern structure
    (let* ((q (cypher-parse "MATCH (a)-[r]->(b), (c) RETURN a"))
           (pat (getf (cdr (second q)) :pattern)))
      (check-equal (length pat) 2 "two chains")
      (let ((chain (first pat)))
        (check-equal (length chain) 3 "chain has 3 elements")
        (check (equal (second chain) (list :rel :var (ast-var "r") :type nil
                                           :dir :out :props nil :min nil :max nil))
               "anonymous typed rel")))
    ;; undirected + in
    (let ((q (cypher-parse "MATCH (a)--(b)<-[r:T]-(c) RETURN a")))
      (let ((chain (first (getf (cdr (second q)) :pattern))))
        (check (eql (getf (cdr (second chain)) :dir) :both) "undirected --")
        (check (eql (getf (cdr (fourth chain)) :dir) :in) "incoming <-")
        (check (equal (getf (cdr (fourth chain)) :type) "T") "rel type")))
    ;; clauses: with/where, unwind, distinct, union
    (let ((q (cypher-parse "MATCH (a) WITH a AS x, count(*) AS n WHERE n > 1 UNWIND x.list AS i RETURN DISTINCT i UNION ALL MATCH (b) RETURN b")))
      (check-equal (length (getf (cdr q) :queries)) 2 "query has union of 2")
      (check (eq (car q) :union) "union head")
      (check (equal (getf (cdr q) :all) t) "union all"))
    ;; create/merge/set/remove/delete
    (let ((q (cypher-parse "CREATE (a:A {x: 1})-[:R {y: 's'}]->(b) SET a.p = 2, b.q += 3, b:B REMOVE a.x DELETE a DETACH DELETE b")))
      (let ((create (second q)) (set (third q)) (remove (fourth q))
            (del (fifth q)) (detach (sixth q)))
        (check (eq (car create) :create) "create clause")
        (check (equal (getf (cdr set) :items)
                      (list (list :set-prop :var (ast-var "a") :prop "p" :expr (list :lit 2))
                            (list :add-prop :var (ast-var "b") :prop "q" :expr (list :lit 3))
                            (list :set-label :var (ast-var "b") :label "B")))
               "set items")
        (check (equal (getf (cdr remove) :items)
                      (list (list :remove-prop :var (ast-var "a") :prop "x")))
               "remove items")
        (check (equal (getf (cdr detach) :detach) t) "detach delete")))
    ;; expressions: precedence, case, predicates, maps/lists
    (check (equal (cypher-parse-expr "1 + 2 * 3")
                  (list :bin :+ (list :lit 1)
                        (list :bin :* (list :lit 2) (list :lit 3))))
           "precedence")
    (check (equal (cypher-parse-expr "NOT a IS NULL AND b IN [1,2]")
                  (list :bin :and
                        (list :not (list :is-null :expr (ast-var "a")))
                        (list :bin :in (ast-var "b")
                              (list :list :items (list (list :lit 1) (list :lit 2))))))
           "not/is null/in/list")
    (check (equal (cypher-parse-expr "CASE x WHEN 1 THEN 'a' ELSE 'b' END")
                  (list :case :base (ast-var "x")
                        :clauses (list (cons (list :lit 1) (list :lit "a")))
                        :else (list :lit "b")))
           "case expression")
    (check (equal (cypher-parse-expr "all(x IN xs WHERE x > 0)")
                  (list :pred :kind :all :var (ast-var "x") :list (ast-var "xs")
                        :pred (list :bin :> (ast-var "x") (list :lit 0))))
           "all predicate")
    (check (equal (cypher-parse-expr "{a: 1, b: 'two'}")
                  (list :map :pairs (list (cons "a" (list :lit 1))
                                          (cons "b" (list :lit "two")))))
           "map literal")
    (check (equal (cypher-parse-expr "n.p[0]")
                  (list :idx :expr (list :prop :expr (ast-var "n") :prop "p")
                        :index (list :lit 0)))
           "property + index")
    (check (equal (cypher-parse-expr "-1")
                  (list :lit -1))
           "negated literal folds")
    ;; error taxonomy
    (flet ((kind-of (s)
             (handler-case (progn (cypher-parse s) nil)
               (cypher-error (e) (cypher-error-kind e)))))
      (check (equal (kind-of "MATCH (a RETURN a") "UnexpectedSyntax") "syntax error")
      (check (equal (kind-of "MATCH (a)-[r:A|B]->(b) RETURN r") "NoSingleRelationshipType")
             "multiple rel types")
      (check (equal (kind-of "MATCH (a) RETURN") "UnexpectedSyntax") "return needs projection")
      (check (equal (kind-of "MATCH (a) MATCH (b) CREATE (c) MATCH (d) RETURN d")
                   "InvalidClauseComposition")
             "match after update"))))

(defun test-cypher-roundtrip ()
  (deftest cypher-roundtrip
    (let ((queries
            (list
             "MATCH (n) RETURN n"
             "MATCH (a:Person)-[r:KNOWS]->(b) WHERE b.age > 30 RETURN a.name AS n, count(*) AS c"
             "MATCH (a)-[r]->(b), (c) RETURN a, r, c ORDER BY a.name DESC, c SKIP 1 LIMIT 5"
             "MATCH (a)--(b)<-[r:T {since: 2020}]-(c) RETURN DISTINCT b"
             "OPTIONAL MATCH (a) WHERE a.x IS NOT NULL WITH a AS x UNWIND [1,2,3] AS i RETURN x, i"
             "CREATE (a:A {x: 1})-[:R]->(b) SET a.p = 2 REMOVE b.q DELETE a"
             "MATCH (a) RETURN a UNION ALL MATCH (b) RETURN b"
             (format nil "RETURN 1 + 2 * 3 AS x, 'str~c'ing' AS s, [1, 2] AS l, {k: 'v'} AS m"
                     (code-char 92))
             "MATCH (n {name: $name}) RETURN CASE n.k WHEN 1 THEN 'a' ELSE 'b' END"
             "MATCH (n) WHERE all(x IN n.list WHERE x > 0) RETURN n"
             "UNWIND range(1, 3) AS i MERGE (n:N {i: i}) RETURN n"
             "MATCH (a) WHERE exists((a)-->(b)) RETURN a")))
      (dolist (q queries)
        (check (equal (cypher-parse (ast-print (cypher-parse q)))
                      (cypher-parse q))
               (format nil "roundtrip: ~a" q))))
    ;; expression roundtrips
    (dolist (e '("1 + 2" "NOT a" "a IS NULL" "b STARTS WITH 'x'" "c ENDS WITH 'y'"
                 "d CONTAINS 'z'" "e IN [1,2]" "-x + 5" "2 ^ 3 ^ 2" "n.a[1].b"
                 "size(list) + length(s)" "coalesce(a, b, 0)" "toInteger('42')"
                 "{a: 1, b: 'two'}" "[1, 2.5, 'three']"))
      (check (equal (cypher-parse-expr (ast-print (list :expr (cypher-parse-expr e))))
                    (cypher-parse-expr e))
             (format nil "expr roundtrip: ~a" e)))))



;;; ------------------------------------------------------------------
;;; cypher executor + reference oracle

(defun %mk-graph ()
  "A: Person Ada -KNOWS-> B: Person Bob (age 42) -KNOWS-> C: Person Cyd;
A -LIKES-> C; C -LIKES-> A; B -WORKS_AT-> org:Company {name: 'Acme'}."
  (let* ((store (make-store))
         (g (make-local-graph store)))
    (let ((a (graph-create-node g :labels '("Person") :props '(("name" . "Ada"))))
          (b (graph-create-node g :labels '("Person") :props '(("name" . "Bob") ("age" . 42))))
          (c (graph-create-node g :labels '("Person") :props '(("name" . "Cyd"))))
          (org (graph-create-node g :labels '("Company") :props '(("name" . "Acme")))))
      (graph-create-relationship g "KNOWS" a b)
      (graph-create-relationship g "KNOWS" b c)
      (graph-create-relationship g "LIKES" a c)
      (graph-create-relationship g "LIKES" c a)
      (graph-create-relationship g "WORKS_AT" b org)
      g)))

(defun %row= (a b)
  (and (= (length a) (length b))
       (every (lambda (p)
                (let ((q (assoc (car p) b)))
                  (and q (let ((e (scalaxy:cypher-= (cdr p) (cdr q))))
                           (not (scalaxy:cypher-null-p e))
                           e))))
              a)))

(defun %rows-set= (a b)
  (and (= (length a) (length b))
       (every (lambda (r) (some (lambda (s) (%row= r s)) b)) a)))

(defun %q (graph query &key (params nil) (matcher :executor))
  (scalaxy:cypher-query query graph :params params :matcher matcher))

(defun %row1 (rows var)
  "The single value of VAR in ROWS (which must have one row)."
  (cdr (assoc var (first rows))))

(defun %names (rows)
  (mapcar (lambda (r) (cdr (assoc (ast-var "name") r))) rows))

(defun test-cypher-executor ()
  (deftest cypher-executor
    (let ((g (%mk-graph)))
      ;; basic match/return with label + property
      (check-equal (length (%q g "MATCH (n:Person) RETURN n")) 3 "match by label")
      (check-equal (length (%q g "MATCH (n:Person {name: 'Ada'}) RETURN n")) 1 "inline property")
      (check-equal (%row1 (%q g "MATCH (n:Person {name: 'Ada'}) RETURN n.name") (ast-var "n.name"))
                   "Ada" "property projection")
      (check-equal (length (%q g "MATCH (n) RETURN n")) 4 "match all nodes")
      ;; relationship direction
      (check-equal (length (%q g "MATCH (a)-[:KNOWS]->(b) RETURN a, b")) 2 "out rels")
      (check-equal (length (%q g "MATCH (a)<-[:KNOWS]-(b) RETURN a, b")) 2 "in rels")
      (check-equal (length (%q g "MATCH (a)-[:KNOWS]-(b) RETURN a, b")) 4 "undirected rels")
      (check-equal (length (%q g "MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a, b, c")) 1
                   "two-hop chain")
      (check-equal (length (%q g "MATCH ()-[r:KNOWS]->() RETURN r")) 2 "anonymous endpoints")
      (check-equal (length (%q g "MATCH ()-[r]->() RETURN r")) 5 "all rels")
      (check-equal (%row1 (%q g "MATCH (:Person {name: 'Bob'})-[r]->() RETURN type(r)")
                          (ast-var "type(r)"))
                   "KNOWS" "type() function")
      ;; homomorphism: variables may rebind to the same node (a = c)
      (check-equal (length (%q g "MATCH (a)-[:LIKES]->(b)-[:LIKES]->(c) RETURN a, b, c")) 2
                   "homomorphism rebinding")
      (check-equal (length (%q g "MATCH (a)-[:LIKES]->(b)-[:LIKES]->(a) RETURN a, b")) 2
                   "cycle patterns")
      ;; where with 3VL: null comparisons drop rows
      (check-equal (length (%q g "MATCH (n) WHERE n.name = 'Ada' RETURN n")) 1 "where eq")
      (check-equal (length (%q g "MATCH (n) WHERE n.age > 40 RETURN n")) 1 "where gt")
      (check-equal (length (%q g "MATCH (n) WHERE n.age > 40 AND n.age < 50 RETURN n")) 1
                   "where and")
      (check-equal (length (%q g "MATCH (n) WHERE n.age = 40 OR n.age = 42 RETURN n")) 1
                   "where or")
      (check-equal (length (%q g "MATCH (n) WHERE NOT n.age = 42 RETURN n")) 0
                   "where not (3VL: NOT null is null, dropped)")
      (check-equal (length (%q g "MATCH (n) WHERE n.name IS NULL RETURN n")) 0 "is null")
      (check-equal (length (%q g "MATCH (n) WHERE n.name IS NOT NULL RETURN n")) 4 "is not null")
      (check-equal (length (%q g "MATCH (n) WHERE n.missing = 1 RETURN n")) 0 "null comparison drops")
      (check-equal (length (%q g "MATCH (n) WHERE n.name STARTS WITH 'A' RETURN n")) 2
                   "starts with")
      (check-equal (length (%q g "MATCH (n) WHERE n.name ENDS WITH 'd' RETURN n")) 1 "ends with")
      (check-equal (length (%q g "MATCH (n) WHERE n.name CONTAINS 'o' RETURN n")) 1 "contains")
      (check-equal (length (%q g "MATCH (n) WHERE n.name IN ['Ada', 'Bob'] RETURN n")) 2 "in list")
      ;; distinct, order, skip, limit
      (check-equal (length (%q g "MATCH (n) RETURN DISTINCT n.name")) 4 "distinct names")
      (check (equal (%names (%q g "MATCH (n) RETURN n.name AS name ORDER BY name"))
                    '("Acme" "Ada" "Bob" "Cyd"))
             "order by asc")
      (check (equal (%names (%q g "MATCH (n) RETURN n.name AS name ORDER BY name DESC"))
                    '("Cyd" "Bob" "Ada" "Acme"))
             "order by desc")
      (check (equal (%names (%q g "MATCH (n) RETURN n.name AS name ORDER BY name SKIP 1 LIMIT 2"))
                    '("Ada" "Bob"))
             "skip + limit")
      ;; unwind, with
      (check-equal (length (%q g "UNWIND [1, 2, 3] AS x RETURN x")) 3 "unwind")
      (check-equal (%row1 (%q g "UNWIND [1, 2, 3] AS x WITH sum(x) AS s RETURN s") (ast-var "s"))
                   6 "with + sum")
      ;; aggregation
      (check-equal (%row1 (%q g "MATCH (n) RETURN count(*)") (ast-var "count(*)")) 4 "count star")
      (check-equal (%row1 (%q g "MATCH (n) RETURN count(n.age)") (ast-var "count(n.age)")) 1
                   "count non-null")
      (check-equal (%row1 (%q g "MATCH (n) RETURN sum(n.age)") (ast-var "sum(n.age)")) 42 "sum")
      (check-equal (%row1 (%q g "MATCH (n) RETURN max(n.age)") (ast-var "max(n.age)")) 42 "max")
      (check (equal (%row1 (%q g "MATCH (n) RETURN collect(n.name)") (ast-var "collect(n.name)"))
                    (cypher-list '("Ada" "Bob" "Cyd" "Acme")))
             "collect")
      (check-equal (length (%q g "MATCH (n) RETURN n.name, count(*) AS c")) 4 "grouping")
      (check-equal (%row1 (%q g "MATCH (n:Missing) RETURN count(*)") (ast-var "count(*)")) 0
                   "count over empty")
      ;; optional match fills with null
      (let ((rows (%q g "MATCH (n:Person) OPTIONAL MATCH (n)-[:WORKS_AT]->(org) RETURN n, org")))
        (check-equal (length rows) 3 "optional match rows")
        (check (= (count-if (lambda (r) (not (scalaxy:cypher-null-p
                                                (cdr (assoc (ast-var "org") r)))))
                           rows)
                   1)
               "one works-at match, two null fills"))
      ;; union
      (check-equal (length (%q g "MATCH (n:Person) RETURN n.name UNION MATCH (n:Company) RETURN n.name"))
                   4 "union dedupes")
      (check-equal (length (%q g "MATCH (n:Person) RETURN n.name UNION ALL MATCH (n:Company) RETURN n.name"))
                   4 "union all")
      ;; exists pattern
      (check-equal (length (%q g "MATCH (n) WHERE exists((n)-[:KNOWS]->()) RETURN n")) 2
                   "exists pattern")
      ;; parameters
      (let ((params (make-hash-table :test #'equal)))
        (setf (gethash "who" params) "Ada")
        (check-equal (length (%q g "MATCH (n) WHERE n.name = $who RETURN n" :params params)) 1
                     "parameters"))
      ;; expressions
      (check-equal (%row1 (%q g "RETURN 1 + 2 * 3 AS x") (ast-var "x")) 7 "arithmetic")
      (check-equal (%row1 (%q g "RETURN coalesce(null, 1, 2) AS x") (ast-var "x")) 1
                   "coalesce")
      (check-equal (%row1 (%q g "RETURN size([1,2,3]) AS x") (ast-var "x")) 3 "size list")
      (check-equal (%row1 (%q g "RETURN head([1,2]) AS x") (ast-var "x")) 1 "head")
      (check-equal (%row1 (%q g "RETURN toInteger('42') AS x") (ast-var "x")) 42
                   "toInteger")
      (check-equal (%row1 (%q g "RETURN [1,2][1] AS x") (ast-var "x")) 2 "list index")
      (check-equal (%row1 (%q g "RETURN {a: 1}.a AS x") (ast-var "x")) 1 "map access")
      (check-equal (%row1 (%q g "RETURN CASE 2 WHEN 1 THEN 'a' WHEN 2 THEN 'b' ELSE 'c' END AS x")
                          (ast-var "x"))
                   "b" "simple case")
      (check-equal (%row1 (%q g "RETURN all(x IN [1,2,3] WHERE x > 0) AS x")
                          (ast-var "x"))
                   t "all predicate")
      (check (handler-case (progn (%q g "RETURN labels({l: 1}) AS x") nil)
               (cypher-error (e) t))
             "labels on map errors (spec)")
      ;; string functions
      (check-equal (%row1 (%q g "RETURN toUpper('ab') AS x") (ast-var "x")) "AB" "toUpper")
      (check-equal (%row1 (%q g "RETURN substring('hello', 1, 3) AS x") (ast-var "x"))
                   "ell" "substring")
      (check (equal (%row1 (%q g "RETURN split('a,b', ',') AS x") (ast-var "x"))
                    (cypher-list '("a" "b")))
             "split"))))

(defun test-cypher-oracle ()
  (deftest cypher-oracle
    (let ((g (%mk-graph)))
      (dolist (q '("MATCH (n) RETURN n"
                   "MATCH (n:Person) RETURN n"
                   "MATCH (a)-[:KNOWS]->(b) RETURN a, b"
                   "MATCH (a)-[:KNOWS]-(b) RETURN a, b"
                   "MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a, b, c"
                   "MATCH (a)-[:LIKES]->(b)-[:LIKES]->(c) RETURN a, b, c"
                   "MATCH (n) WHERE n.age > 40 RETURN n"
                   "MATCH (n:Person) OPTIONAL MATCH (n)-[:WORKS_AT]->(o) RETURN n, o"
                   "MATCH (n) RETURN n.name, count(*) AS c"
                   "MATCH (n) WHERE exists((n)-[:KNOWS]->()) RETURN n"))
        (let ((fast (%q g q :matcher :executor))
              (oracle (%q g q :matcher :reference)))
          (check (%rows-set= fast oracle)
                 (format nil "oracle agreement: ~a" q)))))))



;;; ------------------------------------------------------------------
;;; cypher semantics + update clauses

(defun %err-kind (query &key (graph nil))
  (handler-case (progn (scalaxy:cypher-query query (or graph (%mk-graph))) nil)
    (scalaxy:cypher-error (e) (scalaxy:cypher-error-kind e))))

(defun test-cypher-semantics ()
  (deftest cypher-semantics
    (let ((g (%mk-graph)))
      (check (equal (%err-kind "RETURN undefinedVar" :graph g) "UndefinedVariable")
             "undefined variable in RETURN")
      (check (equal (%err-kind "MATCH (n) WHERE q > 1 RETURN n" :graph g) "UndefinedVariable")
             "undefined variable in WHERE")
      (check (equal (%err-kind "MATCH (n) UNWIND [1] AS n RETURN n" :graph g) nil)
             "unwind rebinding shadows (legal)")
      (check (equal (%err-kind "MATCH (n) WITH n AS a, 1 AS a RETURN a" :graph g)
                   "ColumnNameConflict")
             "duplicate alias in WITH")
      (check (equal (%err-kind "MATCH (n)-[r]->() RETURN r.name" :graph g) nil)
             "no error for valid query")
      (check (equal (%err-kind "MATCH (n) WHERE count(*) > 1 RETURN n" :graph g)
                   "InvalidAggregation")
             "aggregation in WHERE")
      (check (equal (%err-kind "MATCH (n) RETURN n, count(*) SKIP 1 LIMIT 2" :graph g) nil)
             "constant skip/limit ok")
      (check (equal (%err-kind "MATCH (n) RETURN n SKIP n" :graph g) "NonConstantExpression")
             "non-constant SKIP")
      (check (equal (%err-kind "MATCH (a) RETURN a UNION MATCH (b) RETURN b, 1" :graph g)
                   "DifferentColumnsInUnion")
             "union column mismatch"))))

(defun test-cypher-updates ()
  (deftest cypher-updates
    (let* ((store (make-store))
           (g (make-local-graph store)))
      ;; CREATE with labels + props + rel, RETURN the new node
      (let ((rows (%q g "CREATE (a:Person {name: 'Ada'})-[:KNOWS]->(b:Person {name: 'Bob'}) RETURN a.name AS a, b.name AS b")))
        (check-equal (length rows) 1 "create returns one row")
        (check (equal (cdr (assoc (ast-var "a") (first rows))) "Ada") "create a")
        (check (equal (cdr (assoc (ast-var "b") (first rows))) "Bob") "create b"))
      (check-equal (graph-count-nodes g) 2 "two nodes created")
      (check-equal (graph-count-rels g) 1 "one rel created")
      (check-equal (length (%q g "MATCH (n:Person) RETURN n")) 2 "nodes matchable")
      ;; CREATE anchored to an existing variable
      (%q g "MATCH (a:Person {name: 'Ada'}) CREATE (a)-[:LIKES]->(c:Thing {x: 1})")
      (check-equal (length (%q g "MATCH (a:Person {name: 'Ada'})-[:LIKES]->(c) RETURN c")) 1
                   "create anchored to match")
      ;; MERGE: idempotent (law L15)
      (%q g "MERGE (m:Person {name: 'Ada'})")
      (check-equal (length (%q g "MATCH (m:Person {name: 'Ada'}) RETURN m")) 1
                   "merge matched, no duplicate")
      (%q g "MERGE (m:Thing {x: 2})")
      (%q g "MERGE (m:Thing {x: 2})")
      (check-equal (length (%q g "MATCH (m:Thing {x: 2}) RETURN m")) 1 "merge idempotent")
      ;; SET property, label; REMOVE
      (%q g "MATCH (n:Person {name: 'Bob'}) SET n.age = 42, n:Engineer")
      (check (equal (%row1 (%q g "MATCH (n:Person {name: 'Bob'}) RETURN n.age AS x") (ast-var "x"))
                    42)
             "set property")
      (check-equal (length (%q g "MATCH (n:Engineer) RETURN n")) 1 "set label")
      (%q g "MATCH (n:Person {name: 'Bob'}) REMOVE n.age, n:Engineer")
      (check (scalaxy:cypher-null-p
              (%row1 (%q g "MATCH (n:Person {name: 'Bob'}) RETURN n.age AS x") (ast-var "x")))
             "remove property")
      (check-equal (length (%q g "MATCH (n:Engineer) RETURN n")) 0 "remove label")
      ;; SET var = map replaces all properties
      (%q g "MATCH (n:Person {name: 'Bob'}) SET n = {p: 1, q: 'two'}")
      (let ((props (getf (graph-node g (first (graph-scan-node-ids g :label "Person")) ) :props)))
        (declare (ignore props)))
      (check-equal (length (%q g "MATCH (n:Person) WHERE n.p = 1 AND n.q = 'two' AND n.name IS NULL RETURN n"))
                   1
                   "set var = map replaces properties")
      ;; SET += appends to a list
      (%q g "MATCH (n:Person {name: 'Ada'}) SET n.tags = ['a']")
      (%q g "MATCH (n:Person {name: 'Ada'}) SET n.tags += 'b'")
      (check (equal (%row1 (%q g "MATCH (n:Person {name: 'Ada'}) RETURN n.tags AS x") (ast-var "x"))
                    (cypher-list '("a" "b")))
             "+= appends")
      ;; DELETE relationship; DELETE node with rels fails; DETACH DELETE
      (%q g "MATCH (a:Person {name: 'Ada'})-[r:LIKES]->() DELETE r")
      (check-equal (graph-count-rels g) 1 "delete rel")
      (check (handler-case (progn (%q g "MATCH (n:Person {name: 'Ada'}) DELETE n") nil)
               (error () t))
             "delete node with rels fails (A1)")
      (%q g "MATCH (n:Person {name: 'Ada'}) DETACH DELETE n")
      (check-equal (length (%q g "MATCH (n:Person {name: 'Ada'}) RETURN n")) 0
                   "detach delete removes node")
      (check-equal (graph-count-rels g) 0 "detach delete removed incident rel")
      ;; MERGE with relationship
      (%q g "MERGE (a:City {name: 'X'})-[:ROAD]->(b:City {name: 'Y'})")
      (%q g "MERGE (a:City {name: 'X'})-[:ROAD]->(b:City {name: 'Y'})")
      (check-equal (graph-count-rels g) 1 "merge relationship idempotent"))))



;;; ------------------------------------------------------------------
;;; cypher over the wire: TCP client, cluster gateway, REST, console

(defun %cell (table row column)
  "Value of COLUMN in a decoded JSON result table row (positional)."
  (nth (position column (gethash "columns" table) :test #'equal) row))

(defun test-cypher-tcp ()
  (deftest cypher-tcp
    (let* ((node (make-node :id "cyphernode"))
           (server (tcp-serve node :port 0)))
      (unwind-protect
           (let* ((client (connect :host "127.0.0.1" :port (server-port server))))
             ;; seed data through the cypher endpoint itself
             (check (cypher client "CREATE (a:User {name: 'a'})-[:F]->(b:User {name: 'b'})")
                    "cypher over tcp: create")
             (let ((r (cypher client "MATCH (n:User) RETURN n.name AS name ORDER BY name")))
               (check (hash-table-p r) "result is a table")
               (check (= (gethash "count" r) 2) "tcp: two users")
               (check (equal (mapcar (lambda (row) (%cell r row "name"))
                                     (gethash "rows" r))
                             '("a" "b"))
                      "tcp: names"))
             (let ((params (make-hash-table :test #'equal)))
               (setf (gethash "who" params) "b")
               (let ((r (cypher client "MATCH (n:User {name: $who}) RETURN n" :params params)))
                 (check (= (gethash "count" r) 1) "tcp: params")))
             (let ((r (cypher client "MATCH (a)-[:F]->(b) RETURN a.name AS a, b.name AS b")))
               (check (= (gethash "count" r) 1) "tcp: relationship query"))
             ;; per-database isolation over the wire
             (create-database client "cyph2")
             (check (cypher client "CREATE (:X {v: 1})" :db "cyph2") "tcp: db create")
             (check (= (gethash "count"
                                (cypher client "MATCH (n:X) RETURN n" :db "cyph2"))
                       1)
                    "tcp: db query")
             (check (= (gethash "count"
                                (cypher client "MATCH (n:X) RETURN n"))
                       0)
                    "tcp: default db isolated")
             ;; errors come back as error replies
             (let ((reply (tcp-request "127.0.0.1" (server-port server)
                                       (list :op #.+op-cypher+ :db "default"
                                             :query "RETURN broken"
                                             :params (string-to-octets "")))))
               (check (eql (getf reply :op) #.+op-error+) "tcp: syntax error reply")
               (check (search "UndefinedVariable" (getf reply :message))
                      "tcp: error message carries kind")))
        (tcp-stop server)))))

(defun test-cypher-gateway ()
  (deftest cypher-gateway
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "cg~d" i))))
           (servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (peers (loop for i below 3
                        collect (list (format nil "cg~d" i) "127.0.0.1"
                                      (server-port (nth i servers)))))
           (gw (make-gateway :peers peers)))
      (unwind-protect
           (progn
             ;; writes through the gateway graph (ring-routed + replicated)
             (check (null (gateway-cypher gw "CREATE (a:N {name: 'a'})-[:R]->(b:N {name: 'b'}), (c:N {name: 'c'})"))
                    "gateway: create across cluster (no result rows)")
             (let ((rows (gateway-cypher gw "MATCH (n:N) RETURN n.name AS name ORDER BY name")))
               (check (= (length rows) 3) "gateway: all nodes across cluster")
               (check (equal (sort (mapcar (lambda (r) (cdr (assoc (ast-var "name") r))) rows)
                                   #'string<)
                             '("a" "b" "c"))
                      "gateway: names"))
             (let ((rows (gateway-cypher gw "MATCH (a:N {name: 'a'})-[:R]->(b) RETURN b.name AS name")))
               (check (= (length rows) 1) "gateway: relationship traversal"))
             (gateway-cypher gw "MATCH (n:N {name: 'c'}) DELETE n")
             (check (= (length (gateway-cypher gw "MATCH (n:N) RETURN n")) 2)
                    "gateway: delete across cluster"))
        (dolist (s servers) (tcp-stop s))))))

(defun test-cypher-web ()
  (deftest cypher-web
    (let* ((node (make-node :id "cyweb"))
           (http-server (http-serve
                         (make-web-handler :node node :web-dir "web/"
                                           :address "127.0.0.1:7200"
                                           :http-address "127.0.0.1:8080")
                         :port 0)))
      (unwind-protect
           (let ((port (http-server-port http-server)))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"CREATE (:W {x: 1}), (:W {x: 2})\"}")
               (declare (ignore hdrs))
               (check (= status 200) "api cypher create"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"MATCH (n:W) RETURN n.x AS x ORDER BY x\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 200) "api cypher query")
                 (check (= (gethash "count" d) 2) "api cypher count")
                 (check (equal (mapcar (lambda (row) (%cell d row "x")) (gethash "rows" d))
                               '(1 2))
                        "api cypher rows")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"RETURN broken\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 400) "api cypher error status")
                 (check (equal (gethash "kind" d) "UndefinedVariable") "api cypher error kind")))
             ;; console command
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/query"
                               :body "{\"command\":\"cypher MATCH (n:W {x: 1}) RETURN n.x\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (gethash "ok" d) "console cypher ok")
                 (check (search "1 row" (gethash "output" d)) "console cypher output")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/query"
                               :body "{\"command\":\"cypher RETURN broken\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (not (gethash "ok" d)) "console cypher error ok=false")
                 (check (search "cypher error" (gethash "output" d)) "console cypher error text")))
             ;; per-db cypher over REST
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"CREATE (:Q)\",\"db\":\"restdb\"}")
               (declare (ignore hdrs))
               (check (= status 200) "api cypher per-db create"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"MATCH (n:Q) RETURN n\",\"db\":\"restdb\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= (gethash "count" d) 1) "api cypher per-db query")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/cypher"
                               :body "{\"query\":\"MATCH (n:Q) RETURN n\"}")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= (gethash "count" d) 0) "api cypher default db isolated"))))
        (http-stop http-server)))))


;;; ------------------------------------------------------------------
;;; multiple databases per cluster

(defun test-databases ()
  (deftest databases
    ;; physical-key namespacing round trips
    (check-equal (db-key "default" "k") "d:default:k" "db-key default")
    (check-equal (db-key "analytics" "user:42") "d:analytics:user:42" "db-key namespaced")
    (check-equal (db-parse-name "d:analytics:user:42") "analytics" "db-parse-name")
    (check (null (db-parse-name "user:42")) "db-parse-name plain key")
    (check-equal (db-strip "analytics" "d:analytics:user:42") "user:42" "db-strip")
    (check (null (db-strip "other" "d:analytics:user:42")) "db-strip wrong db")
    (check (db-valid-name-p "my-db_2") "valid db name")
    (check (not (db-valid-name-p "bad name")) "invalid db name (space)")
    (check (not (db-valid-name-p "")) "invalid db name (empty)")
    ;; in-process cluster: per-db isolation
    (let ((cluster (make-cluster :ids '("db1" "db2" "db3"))))
      (cluster-put cluster (db-key "a" "shared") (string-to-octets "in-a"))
      (cluster-put cluster (db-key "b" "shared") (string-to-octets "in-b"))
      (check-equal (octets-to-string (cluster-get cluster (db-key "a" "shared"))) "in-a"
                   "db a value")
      (check-equal (octets-to-string (cluster-get cluster (db-key "b" "shared"))) "in-b"
                   "db b value")
      (check-equal (length (cluster-scan cluster (db-key "a" ""))) 1 "scan scoped to db a")
      (check-equal (length (cluster-scan cluster (db-key "b" ""))) 1 "scan scoped to db b")
      (cluster-create-database cluster "newdb")
      (check (member "newdb" (cluster-list-databases cluster) :test #'equal)
             "created db is listed")
      (cluster-put cluster (db-key "newdb" "k") (string-to-octets "v"))
      (cluster-drop-database cluster "newdb")
      (check (not (member "newdb" (cluster-list-databases cluster) :test #'equal))
             "dropped db is gone")
      (check (null (cluster-get cluster (db-key "newdb" "k"))) "dropped db data gone")
      (check-equal (octets-to-string (cluster-get cluster (db-key "a" "shared"))) "in-a"
                   "other dbs untouched by drop"))
    ;; client API over TCP with multiple databases
    (let* ((node (make-node :id "dbnode"))
           (server (tcp-serve node :port 0)))
      (unwind-protect
           (let* ((client (connect :host "127.0.0.1" :port (server-port server))))
             (check (put client "k" "default-value") "put in default db")
             (check (create-database client "analytics") "create database")
             (check (put client "k" "analytics-value" :db "analytics") "put in analytics")
             (check-equal (octets-to-string (get client "k")) "default-value"
                          "get in default db")
             (check-equal (octets-to-string (get client "k" :db "analytics")) "analytics-value"
                          "get in analytics db")
             (check (equal (sort (list-databases client) #'string<)
                           '("analytics" "default"))
                    "list databases")
             (check-equal (length (scan client "")) 1 "default scan sees only own keys")
             (check-equal (length (scan client "" :db "analytics")) 1 "analytics scan isolated")
             (delete client "k" :db "analytics")
             (check (null (get client "k" :db "analytics")) "delete in analytics")
             (check-equal (octets-to-string (get client "k")) "default-value"
                          "default db untouched"))
        (tcp-stop server))))
  (deftest databases-web
    (let* ((node (make-node :id "dbweb"))
           (http-server (http-serve
                         (make-web-handler :node node :web-dir "web/"
                                           :address "127.0.0.1:7200"
                                           :http-address "127.0.0.1:8080")
                         :port 0)))
      (unwind-protect
           (let ((port (http-server-port http-server)))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "POST" "/api/databases"
                               :body "{\"name\":\"webdb\"}")
               (declare (ignore hdrs))
               (check (= status 200) "create database over http")
               (check (gethash "ok" (json-decode body)) "create database ok"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "GET" "/api/databases")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 200) "list databases status")
                 (check (member "webdb"
                                (mapcar (lambda (x) (gethash "name" x))
                                        (gethash "databases" d))
                                :test #'equal)
                        "created db listed")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "PUT" "/api/keys/k1?db=webdb"
                               :body "{\"value\":\"in-webdb\"}")
               (declare (ignore hdrs))
               (check (= status 200) "put into webdb"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "GET" "/api/keys/k1?db=webdb")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= status 200) "get from webdb")
                 (check (string= (gethash "utf8" d) "in-webdb") "webdb value")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "GET" "/api/keys/k1")
               (declare (ignore hdrs))
               (check (= status 404) "default db does not see webdb key"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "GET" "/api/keys?db=webdb")
               (declare (ignore hdrs))
               (let ((d (json-decode body)))
                 (check (= (gethash "total" d) 1) "webdb keys listing")
                 (check (string= (gethash "key" (first (gethash "keys" d))) "k1")
                        "webdb listing shows logical key")))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "DELETE" "/api/databases/webdb")
               (declare (ignore hdrs))
               (check (= status 200) "drop database over http"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "GET" "/api/keys/k1?db=webdb")
               (declare (ignore hdrs))
               (check (= status 404) "webdb key gone after drop"))
             (multiple-value-bind (status hdrs body)
                 (http-request "127.0.0.1" port "DELETE" "/api/databases/default")
               (declare (ignore hdrs))
               (check (= status 400) "default db cannot be dropped")))
        (http-stop http-server)))))

;;; ------------------------------------------------------------------
;;; multiple databases through the gateway (multi-node TCP)

(defun test-databases-gateway ()
  (deftest databases-gateway
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "d~d" i))))
           (servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (peers (loop for i below 3
                        collect (list (format nil "d~d" i) "127.0.0.1"
                                      (server-port (nth i servers)))))
           (gw (make-gateway :peers peers)))
      (unwind-protect
           (progn
             (check (gateway-create-database gw "metrics") "gateway create database")
             (gateway-put gw (db-key "metrics" "cpu") (string-to-octets "42"))
             (gateway-put gw (db-key "default" "cpu") (string-to-octets "1"))
             (check (member "metrics" (gateway-list-databases gw) :test #'equal)
                    "gateway list databases")
             (check-equal (length (gateway-scan gw (db-key "metrics" ""))) 2
                          "gateway db scan (data + marker)")
             (gateway-drop-database gw "metrics")
             (check (not (member "metrics" (gateway-list-databases gw) :test #'equal))
                    "gateway drop database")
             (check (null (gateway-get gw (db-key "metrics" "cpu"))) "dropped data gone")
             (check-equal (octets-to-string (gateway-get gw (db-key "default" "cpu"))) "1"
                          "other database intact"))
        (dolist (s servers) (tcp-stop s))))))

;;; ------------------------------------------------------------------
;;; value codec

(defun %codec-roundtrip (v &optional (msg "codec roundtrip"))
  (multiple-value-bind (decoded pos) (codec-decode (codec-encode v))
    (check-equal pos (length (codec-encode v)) (format nil "~a (consumed all)" msg))
    (check (cypher-value= decoded v) (format nil "~a (value preserved)" msg))))

(defun test-codec ()
  (deftest codec
    (%codec-roundtrip :cypher-null "codec null")
    (%codec-roundtrip t "codec true")
    (%codec-roundtrip :cypher-false "codec false")
    (%codec-roundtrip 0 "codec int 0")
    (%codec-roundtrip 42 "codec int 42")
    (%codec-roundtrip -42 "codec int -42")
    (%codec-roundtrip 4611686018427387904 "codec int large")
    (%codec-roundtrip -4611686018427387905 "codec int large negative")
    (%codec-roundtrip 1.5 "codec float 1.5")
    (%codec-roundtrip -0.25d0 "codec float -0.25")
    (%codec-roundtrip "hello" "codec string")
    (%codec-roundtrip "unicode \u2192 \u00e9\u00fc" "codec unicode string")
    (%codec-roundtrip #() "codec empty list")
    (%codec-roundtrip (cypher-list (list 1 2 3)) "codec list")
    (%codec-roundtrip (cypher-list (list 1 (cypher-list (list 2 3)) "x")) "codec nested list")
    (%codec-roundtrip (cypher-map (list (cons "a" 1) (cons "b" "two"))) "codec map")
    (%codec-roundtrip
     (cypher-map (list (cons "n" (cypher-map (list (cons "k" (cypher-list (list 1 2))))))))
     "codec nested map")
    (%codec-roundtrip (make-array 0 :element-type (quote (unsigned-byte 8))) "codec empty bytes")
    (let ((blob (make-array 300 :element-type (quote (unsigned-byte 8)))))
      (loop for i below 300 do (setf (aref blob i) (mod i 256)))
      (%codec-roundtrip blob "codec bytes"))
    (%codec-roundtrip
     (cypher-list (list 1 "two" :cypher-null t :cypher-false 1.5
                        (cypher-list (list 1))
                        (cypher-map (list (cons "k" (cypher-list (list 9)))))))
     "codec mixed")
    (check (not (cypher-value= 1 1.0)) "int /= float")
    (check (not (cypher-value= :cypher-null t)) "null /= true")
    (check (cypher-value= #() #()) "empty list = empty list")
    (check (not (cypher-value= #() (cypher-list (list 1)))) "empty list /= [1]")
    (check (handler-case (progn (codec-encode nil) nil)
             (error () t))
           "codec rejects CL NIL")))

;;; ------------------------------------------------------------------
;;; graph storage

(defun %props-of (node)
  (getf node :props))

(defun %prop (node k)
  (cdr (assoc k (getf node :props) :test #'equal)))

(defun test-graph-storage ()
  (deftest graph-storage
    (let* ((store (make-store))
           (g (make-local-graph store)))
      ;; nodes, labels, properties
      (let* ((a (graph-create-node g :labels '("Person") :props '(("name" . "Ada"))))
             (b (graph-create-node g :labels '("Person" "Engineer")
                                     :props '(("name" . "Bob") ("tags" . #())))))
        (check-equal (length (graph-scan-node-ids g)) 2 "two nodes")
        (check-equal (length (graph-scan-node-ids g :label "Person")) 2 "label Person: 2")
        (check-equal (length (graph-scan-node-ids g :label "Engineer")) 1 "label Engineer: 1")
        (check-equal (length (graph-scan-node-ids g :label "Missing")) 0 "label Missing: 0")
        (check-equal (getf (graph-node g a) :labels) '("Person") "node labels")
        (check (equal (%prop (graph-node g a) "name") "Ada") "node property")
        (check (cypher-empty-list-p (%prop (graph-node g b) "tags")) "empty list property")
        (check (cypher-null-p (graph-node-property g a "missing")) "missing property is null")
        ;; property updates
        (graph-set-node-property g a "age" 42)
        (check (equal (%prop (graph-node g a) "age") 42) "set property")
        (graph-set-node-property g a "age" 43)
        (check (equal (%prop (graph-node g a) "age") 43) "overwrite property")
        (graph-remove-node-property g a "age")
        (check (cypher-null-p (graph-node-property g a "age")) "removed property is null")
        ;; labels
        (graph-add-node-label g b "Manager")
        (check (member "Manager" (getf (graph-node g b) :labels) :test #'equal) "label added")
        (check-equal (length (graph-scan-node-ids g :label "Manager")) 1 "label index updated")
        (graph-remove-node-label g b "Manager")
        (check (not (member "Manager" (getf (graph-node g b) :labels) :test #'equal))
               "label removed")
        ;; relationships + expansion
        (let* ((c (graph-create-node g :labels '("Person") :props '(("name" . "Cyd"))))
               (r1 (graph-create-relationship g "KNOWS" a b :props '(("since" . 2019))))
               (r2 (graph-create-relationship g "KNOWS" b c :props '(("since" . 2020))))
               (r3 (graph-create-relationship g "LIKES" c a)))
          (check-equal (length (graph-scan-rel-ids g)) 3 "three rels")
          (check-equal (length (graph-scan-rel-ids g :type "KNOWS")) 2 "rel type filter")
          (check-equal (graph-expand g a :dir :out)
                       (list (cons r1 b)) "expand out")
          (check-equal (graph-expand g b :dir :in)
                       (list (cons r1 a)) "expand in")
          (check-equal (graph-expand g b :dir :both)
                       (sort (list (cons r1 a) (cons r2 c)) #'string< :key #'car)
                       "expand both")
          (check-equal (graph-expand g a :dir :out :type "KNOWS")
                       (list (cons r1 b)) "expand type filter")
          (check (null (graph-expand g a :dir :out :type "LIKES")) "expand type mismatch")
          (check-equal (graph-expand g c :dir :out) (list (cons r3 a)) "expand from c")
          ;; relationship record
          (let ((rel (graph-relationship g r1)))
            (check (equal (getf rel :type) "KNOWS") "rel type")
            (check (equal (getf rel :start) a) "rel start")
            (check (equal (getf rel :end) b) "rel end")
            (check (equal (%prop rel "since") 2019) "rel property"))
          ;; relationship property update
          (graph-set-relationship-property g r1 "since" 2021)
          (check (equal (%prop (graph-relationship g r1) "since") 2021) "rel prop update")
          ;; self-loop does not double-count in :both
          (let ((r4 (graph-create-relationship g "SELF" c c)))
            (check-equal (graph-expand g c :dir :both)
                         (sort (list (cons r2 b) (cons r3 a) (cons r4 c)) #'string< :key #'car)
                         "self-loop counted once in both")
            (graph-delete-relationship g r4))
          ;; delete relationship cleans adjacency
          (graph-delete-relationship g r2)
          (check (null (graph-relationship g r2)) "deleted rel gone")
          (check (null (graph-expand g b :dir :out)) "adjacency cleaned after rel delete")
          ;; delete node without detach fails (axiom A1)
          (check (handler-case (progn (graph-delete-node g b) nil)
                   (error () t))
                 "delete node with rels requires detach")
          ;; detach delete removes node, rels, adjacency, labels
          (graph-delete-node g b :detach t)
          (check (null (graph-node g b)) "detached node gone")
          (check (null (graph-relationship g r1)) "incident rel gone after detach")
          (check (null (graph-expand g a :dir :out)) "adjacency gone after detach")
          (check-equal (length (graph-scan-node-ids g :label "Person")) 2 "label index updated")
          (check-equal (length (graph-scan-rel-ids g)) 1 "one rel remains"))
        ;; invariants hold after all this
        (check (null (graph-check-invariants g)) "invariants hold after mutations")))))

(defun test-graph-blobs ()
  (deftest graph-blobs
    (let* ((store (make-store))
           (g (make-local-graph store)))
      (let* ((small (make-array 512 :element-type '(unsigned-byte 8)))
             (large (make-array 4096 :element-type '(unsigned-byte 8)))
             (n (graph-create-node g :labels '("Doc"))))
        (loop for i below 512 do (setf (aref small i) (mod i 256)))
        (loop for i below 4096 do (setf (aref large i) (mod (* i 7) 256)))
        ;; small blob is inlined in the record
        (graph-set-node-property g n "thumb" small)
        (check (null (g-get g (format nil "b:~a:thumb" n))) "small blob not spilled")
        (check (cypher-value= (%prop (graph-node g n) "thumb") small) "small blob roundtrip")
        ;; large blob is spilled to its own key
        (graph-set-node-property g n "image" large)
        (check (g-get g (format nil "b:~a:image" n)) "large blob spilled to own key")
        (let ((rec (g-get g (format nil "n:~a" n))))
          (check (< (length rec) (+ +blob-inline-limit+ 256)) "record stays small"))
        (check (cypher-value= (%prop (graph-node g n) "image") large) "large blob roundtrip")
        ;; blob spill on create
        (let ((m (graph-create-node g :labels '("Doc")
                                      :props `(("image" . ,large)))))
          (check (g-get g (format nil "b:~a:image" m)) "create spills blob")
          (check (cypher-value= (%prop (graph-node g m) "image") large) "create blob roundtrip"))
        ;; overwrite with small value deletes the spilled blob
        (graph-set-node-property g n "image" small)
        (check (null (g-get g (format nil "b:~a:image" n))) "overwrite deletes spilled blob")
        (check (cypher-value= (%prop (graph-node g n) "image") small) "overwrite value")
        ;; property removal deletes the spilled blob
        (graph-set-node-property g n "image" large)
        (check (g-get g (format nil "b:~a:image" n)) "spilled again")
        (graph-remove-node-property g n "image")
        (check (null (g-get g (format nil "b:~a:image" n))) "remove deletes spilled blob")
        ;; relationship blobs
        (let* ((m (graph-create-node g :labels '("Doc")))
               (r (graph-create-relationship g "HAS" n m :props `(("payload" . ,large)))))
          (check (g-get g (format nil "b:~a:payload" r)) "rel blob spilled")
          (check (cypher-value= (%prop (graph-relationship g r) "payload") large)
                 "rel blob roundtrip"))
        ;; deleting an element deletes its blobs
        (let ((m (graph-create-node g :labels '("Doc") :props `(("image" . ,large)))))
          (check (g-get g (format nil "b:~a:image" m)) "blob exists")
          (graph-delete-node g m)
          (check (null (g-get g (format nil "b:~a:image" m))) "blob deleted with node"))
        (check (null (graph-check-invariants g)) "invariants hold with blobs")))))

(defun test-graph-persistence ()
  (deftest graph-persistence
    (let* ((dir (format nil "/tmp/scalaxy-graph-~d" (get-universal-time)))
           (path (format nil "~a/store.log" dir)))
      (ensure-directories-exist path)
      (unwind-protect
           (progn
             (let* ((store (make-store :path path))
                    (g (make-local-graph store)))
               (let ((a (graph-create-node g :labels '("A") :props '(("v" . 1))))
                     (b (graph-create-node g :labels '("B"))))
                 (graph-create-relationship g "T" a b :props '(("w" . "x")))))
             ;; reopen: replay log, rebuild indexes from scratch
             (let* ((store2 (make-store :path path))
                    (g2 (make-local-graph store2)))
               (check-equal (graph-count-nodes g2) 2 "nodes survived restart")
               (check-equal (graph-count-rels g2) 1 "rels survived restart")
               (check-equal (length (graph-scan-node-ids g2 :label "A")) 1
                            "label index rebuilt after restart")
               (check (null (graph-check-invariants g2)) "invariants after restart")))
        (delete-file path)))))

(defun test-graph-multidb ()
  (deftest graph-multidb
    (let* ((store (make-store))
           (ga (make-local-graph store :db "alpha"))
           (gb (make-local-graph store :db "beta")))
      (let ((a (graph-create-node ga :labels '("X") :props '(("n" . "a"))))
            (b (graph-create-node gb :labels '("X") :props '(("n" . "b")))))
        (check-equal (graph-count-nodes ga) 1 "alpha has its node")
        (check-equal (graph-count-nodes gb) 1 "beta has its node")
        (check (equal (%prop (graph-node ga a) "n") "a") "alpha value")
        (check (equal (%prop (graph-node gb b) "n") "b") "beta value")
        (check (equal (getf (graph-node gb b) :id) b) "beta sees its own id")
        (check-equal (length (graph-scan-node-ids ga)) 1 "alpha scan sees one node")
        (check-equal (length (graph-scan-node-ids gb)) 1 "beta scan sees one node"))
      (check (member "alpha" (db-list (store-scan store "d:")) :test #'equal :key #'car)
             "alpha db listed"))))

(defun test-graph-gateway ()
  (deftest graph-gateway
    (let* ((nodes (loop for i below 3 collect (make-node :id (format nil "g~d" i))))
           (servers (loop for n in nodes collect (tcp-serve n :port 0)))
           (peers (loop for i below 3
                        collect (list (format nil "g~d" i) "127.0.0.1"
                                      (server-port (nth i servers)))))
           (gw (make-gateway :peers peers))
           (g (make-gateway-graph gw :db "graphdb")))
      (unwind-protect
           (progn
             (let* ((a (graph-create-node g :labels '("User") :props '(("name" . "a"))))
                    (b (graph-create-node g :labels '("User") :props '(("name" . "b"))))
                    (c (graph-create-node g :labels '("User") :props '(("name" . "c"))))
                    (r (graph-create-relationship g "FOLLOWS" a b)))
               (graph-create-relationship g "FOLLOWS" b c)
               (check-equal (graph-count-nodes g) 3 "gateway: three nodes across cluster")
               (check-equal (length (graph-scan-node-ids g :label "User")) 3
                            "gateway: label scan across cluster")
               (check-equal (graph-expand g a :dir :out)
                            (list (cons r b)) "gateway: expand")
               (check-equal (graph-expand g b :dir :in)
                            (list (cons r a)) "gateway: expand in")
               (check (equal (%prop (graph-node g a) "name") "a") "gateway: node read")
               (check (null (graph-check-invariants g)) "gateway: invariants")
               (graph-delete-node g b :detach t)
               (check-equal (graph-count-rels g) 0 "gateway: detach delete across cluster")))
        (dolist (s servers) (tcp-stop s))))))

;;; ------------------------------------------------------------------
;;; TCK subset (full corpus runs via scripts/run-tck.lisp)

(defun test-tck-subset ()
  (deftest tck-subset
    ;; The full TCK corpus runs via scripts/run-tck.lisp (certification
    ;; step); the runner lives in tests/tck.lisp, loaded as source.
    (check (probe-file "tests/tck.lisp") "tck runner present")
    (check (probe-file "specs/openCypher/tck/features") "tck corpus present")))

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
  (test-codec)
  (test-cypher-lexer)
  (test-cypher-parser)
  (test-cypher-roundtrip)
  (test-cypher-executor)
  (test-cypher-oracle)
  (test-cypher-semantics)
  (test-cypher-updates)
  (test-cypher-tcp)
  (test-cypher-gateway)
  (test-cypher-web)
  (test-tck-subset)
  (test-graph-storage)
  (test-graph-blobs)
  (test-graph-persistence)
  (test-graph-multidb)
  (test-graph-gateway)
  (test-databases)
  (test-databases-gateway)
  (test-gateway)
  (test-gateway-status)
  (test-gateway-failover)
  (format t "~&~%Ran ~d checks, ~d failure~:p.~%" *checks* *failures*)
  (if (zerop *failures*) 0 1))

