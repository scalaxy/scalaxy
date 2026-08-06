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

(defun run-all-tests ()
  (setf *checks* 0 *failures* 0)
  (test-consistent-hash)
  (test-storage)
  (test-protocol)
  (test-replication)
  (test-cluster)
  (test-tcp)
  (format t "~&~%Ran ~d checks, ~d failure~:p.~%" *checks* *failures*)
  (if (zerop *failures*) 0 1))
