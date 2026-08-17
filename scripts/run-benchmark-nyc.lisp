;;;; scripts/run-benchmark-nyc.lisp --- load the NYC taxi graph and time a
;;;; query suite against the Scalaxy Cypher engine.
;;;;
;;;; Usage: sbcl --script scripts/run-benchmark-nyc.lisp [mode] [trip-limit] [iterations]
;;;;   mode: aggregated (default) | per-trip
;;;;   trip-limit: cap on trip relationships loaded (default: all)
;;;;   iterations: runs per query (default: 20)

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")
(in-package #:scalaxy)

(load (merge-pathnames "../benchmarks/nyc-taxi/load.lisp" *load-truename*))
(load (merge-pathnames "../benchmarks/nyc-taxi/queries.lisp" *load-truename*))

(defun %bench-median (times)
  (let ((v (sort (copy-seq times) #'<)))
    (elt v (floor (length v) 2))))

(let* ((argv sb-ext:*posix-argv*)
       (mode (if (and (second argv) (string-equal (second argv) "per-trip"))
                 :per-trip :aggregated))
       (limit (let ((x (third argv)))
                (and x (parse-integer x :junk-allowed t))))
       (iterations (or (parse-integer (or (fourth argv) "20") :junk-allowed t) 20))
       (store (make-store))
       (graph (make-local-graph store))
       (data-dir (merge-pathnames "../benchmarks/nyc-taxi/"
                                  (uiop:pathname-directory-pathname *load-truename*)))
       (queries (if (eq mode :per-trip)
                    *nyc-taxi-queries-per-trip*
                    *nyc-taxi-queries-aggregated*)))
  (format t "~&Scalaxy Cypher benchmark: NYC taxi graph (~a)~%"
          (if (eq mode :per-trip) "per trip" "zone-pair aggregated"))
  (format t "  data: ~a~%" (namestring data-dir))
  (format t "  iterations per query: ~d~%~%" iterations)

  (let ((t0 (get-internal-real-time)))
    (multiple-value-bind (zones trips)
        (load-nyc-taxi graph data-dir :mode mode :limit limit)
      (format t "load: ~d zones, ~d trip relationships in ~,2f s~%"
              zones trips
              (/ (- (get-internal-real-time) t0)
                 internal-time-units-per-second)))
    (format t "nodes: ~d~%" (length (graph-scan-node-ids graph)))
    (format t "relationships: ~d~%~%"
            (length (graph-scan-rel-ids graph))))

  (format t "~&~48a ~10a ~10a~%" "query" "rows" "median")
  (dolist (entry queries)
    (let ((name (car entry)) (cql (cdr entry)) (times nil) (rows 0))
      (handler-case
          (dotimes (i iterations)
            (let ((t0 (get-internal-real-time)))
              (setf rows (length (cypher-query cql graph)))
              (push (* 1000.0 (/ (- (get-internal-real-time) t0)
                                 internal-time-units-per-second))
                    times)))
        (error (e)
          (format t "~&~48a FAILED: ~a~%" name e)))
      (unless (null times)
        (format t "~&~48a ~10d ~9,2fms~%" name rows (%bench-median times)))))
  (format t "~%~&Done.~%"))
