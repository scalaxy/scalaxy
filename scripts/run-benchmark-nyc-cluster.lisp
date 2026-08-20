;;;; scripts/run-benchmark-nyc-cluster.lisp --- three-node S3-backed NYC benchmark
;;;;
;;;; Environment: SCALAXY_S3_ENDPOINT, SCALAXY_S3_BUCKET_0..2,
;;;; SCALAXY_S3_ACCESS_KEY_0..2, SCALAXY_S3_SECRET_KEY_0..2.
;;;; Usage: sbcl --dynamic-space-size 8192 --script scripts/run-benchmark-nyc-cluster.lisp [mode] [limit] [iterations]

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")
(in-package #:scalaxy)
(load (merge-pathnames "../benchmarks/nyc-taxi/load.lisp" *load-truename*))
(load (merge-pathnames "../benchmarks/nyc-taxi/queries.lisp" *load-truename*))

(defclass cluster-benchmark-graph (local-graph-view)
  ((cluster :initarg :cluster :reader benchmark-cluster)))
(defmethod g-put ((g cluster-benchmark-graph) key value)
  (cluster-put (benchmark-cluster g) (db-key (graph-db g) key) value))
(defmethod g-get ((g cluster-benchmark-graph) key)
  (cluster-get (benchmark-cluster g) (db-key (graph-db g) key)))
(defmethod g-delete ((g cluster-benchmark-graph) key)
  (cluster-delete (benchmark-cluster g) (db-key (graph-db g) key)))
(defmethod g-scan ((g cluster-benchmark-graph) prefix)
  (loop for (key . value) in (cluster-scan (benchmark-cluster g) (db-key (graph-db g) prefix))
        collect (cons (db-strip (graph-db g) key) value)))
(defmethod g-counter ((g cluster-benchmark-graph) key)
  (let* ((full (db-key (graph-db g) key))
         (old (or (cluster-get (benchmark-cluster g) full) (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
         (next (1+ (read-u64 old 0)))
         (buf (make-buffer)))
    (buf-write-u64 buf next)
    (cluster-put (benchmark-cluster g) full buf)
    next))

(defun env-required (name)
  (or #+sbcl (sb-ext:posix-getenv name)
      (error "missing required environment variable ~a" name)))
(defun env-or (name default)
  (or #+sbcl (sb-ext:posix-getenv name) default))
(defun median (times)
  (let ((v (sort (copy-seq times) #'<))) (elt v (floor (length v) 2))))

(let* ((argv sb-ext:*posix-argv*)
       (mode (if (and (second argv) (string-equal (second argv) "per-trip")) :per-trip :aggregated))
       (limit (and (third argv) (parse-integer (third argv) :junk-allowed t)))
       (iterations (if (fourth argv)
                      (parse-integer (fourth argv) :junk-allowed t)
                      3))
       (endpoint (env-or "SCALAXY_S3_ENDPOINT" "http://127.0.0.1:3900"))
       (ids '("node-0" "node-1" "node-2"))
       (cluster (make-cluster :ids ids :replicas 1))
       (stores (loop for i below 3
                     collect (make-store
                              :s3-endpoint endpoint
                              :s3-bucket (env-required (format nil "SCALAXY_S3_BUCKET_~d" i))
                              :s3-access-key (env-required (format nil "SCALAXY_S3_ACCESS_KEY_~d" i))
                              :s3-secret-key (env-required (format nil "SCALAXY_S3_SECRET_KEY_~d" i))
                              :s3-region (env-or "SCALAXY_S3_REGION" "us-east-1")
                              :s3-prefix (format nil "benchmark-~a/" (or #+sbcl (sb-ext:posix-getenv "SCALAXY_S3_RUN") "run")))))
       (queries (if (eq mode :per-trip) *nyc-taxi-queries-per-trip* *nyc-taxi-queries-aggregated*)))
  (loop for id in ids for store in stores
        do (setf (node-store (gethash id (cluster-nodes cluster))) store))
  (let ((graph (make-instance 'cluster-benchmark-graph
                              :cluster cluster :store (first stores) :db +default-db+)))
    (graph-rebuild-indexes graph)
    (format t "~&Scalaxy NYC taxi benchmark: 3-node S3 cluster (~a)~%" mode)
    (format t "  endpoint: ~a~%  iterations: ~d~%  limit: ~a~%" endpoint iterations (or limit "all"))
    (let ((t0 (get-internal-real-time)))
      (multiple-value-bind (zones trips)
          (with-s3-batch ()
            (load-nyc-taxi graph (merge-pathnames "../benchmarks/nyc-taxi/"
                                                   (uiop:pathname-directory-pathname *load-truename*))
                           :mode mode :limit limit))
        (format t "  load: ~d zones, ~d trips in ~,2f s~%"
                zones trips (/ (- (get-internal-real-time) t0) internal-time-units-per-second))))
    (format t "  nodes: ~d~%  relationships: ~d~%~%"
            (length (graph-scan-node-ids graph)) (length (graph-scan-rel-ids graph)))
    (when (plusp iterations)
      (format t "~&~48a ~10a ~10a~%" "query" "rows" "median")
      (dolist (entry queries)
      (let ((times nil) (rows 0))
        (handler-case
            (dotimes (i iterations)
              (let ((t0 (get-internal-real-time)))
                (setf rows (length (cypher-query (cdr entry) graph)))
                (push (* 1000.0 (/ (- (get-internal-real-time) t0) internal-time-units-per-second)) times)))
          (error (e) (format t "~&~48a FAILED: ~a~%" (car entry) e)))
        (when times (format t "~&~48a ~10d ~9,2fms~%" (car entry) rows (median times))))))))
