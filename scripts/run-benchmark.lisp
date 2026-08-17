;;;; scripts/run-benchmark.lisp --- load the Neo4j Movie Graph and time a
;;;; query suite against the Scalaxy Cypher engine.
;;;;
;;;; Usage: sbcl --script scripts/run-benchmark.lisp [iterations]

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")
(in-package #:scalaxy)

(load (merge-pathnames "../benchmarks/movies/queries.lisp" *load-truename*))

(defun %bench-split-statements (text)
  "Split a Neo4j .cypher script into ;-terminated statements."
  (let ((out nil) (start 0) (n (length text)))
    (loop for i from 0 below n
          when (char= (char text i) #\;)
            do (let ((stmt (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (subseq text start i))))
                 (when (plusp (length stmt))
                   (push stmt out))
                 (setf start (1+ i))))
    (let ((tail (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (subseq text start))))
      (when (plusp (length tail))
        (push tail out)))
    (nreverse out)))

(defun %bench-strip-comments (text)
  "Remove // comment lines; returns the remaining text."
  (with-output-to-string (out)
    (dolist (line (split-sequence-on #\Newline text))
      (unless (or (search "//" line) (zerop (length (string-trim '(#\Space #\Tab) line))))
        (write-string line out)
        (write-char #\Newline out)))))

(defun %bench-load-movies (graph path)
  "Load the Movie Graph into GRAPH.  Returns (values statements skipped)."
  (let ((text (with-open-file (in path :external-format :utf-8)
                (let ((s (make-string (file-length in))))
                  (read-sequence s in)
                  s)))
        (n 0) (skipped 0))
    (dolist (stmt (%bench-split-statements text))
      (let ((clean (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (%bench-strip-comments stmt))))
        (cond
          ((zerop (length clean)) (incf skipped))
          ((or (search "CREATE CONSTRAINT" clean)
               (search "CREATE INDEX" clean)
               (search "DROP " clean))
           (incf skipped))
          (t (cypher-query clean graph) (incf n)))))
    (values n skipped)))

(defun %bench-median (times)
  (let ((v (sort (copy-seq times) #'<)))
    (elt v (floor (length v) 2))))

(let* ((iterations (or (parse-integer (or (second sb-ext:*posix-argv*) "20")
                                      :junk-allowed t)
                       20))
       (store (make-store))
       (graph (make-local-graph store))
       (data-path (merge-pathnames "../benchmarks/movies/movies.cypher"
                                   *load-truename*)))
  (format t "~&Scalaxy Cypher benchmark: Neo4j Movie Graph~%")
  (format t "  data: ~a~%" (namestring data-path))
  (format t "  iterations per query: ~d~%~%" iterations)

  (let ((t0 (get-internal-real-time)))
    (multiple-value-bind (n skipped) (%bench-load-movies graph data-path)
      (format t "load: ~d statements (~d schema statements skipped) in ~,2f ms~%"
              n skipped
              (* 1000.0 (/ (- (get-internal-real-time) t0)
                           internal-time-units-per-second))))
    (format t "nodes: ~d~%" (length (graph-scan-node-ids graph)))
    (format t "relationships: ~d~%"
            (length (graph-scan-rel-ids graph)))
    (format t "node labels: Movie / Person~%~%"))

  (format t "~&~48a ~8a ~10a ~8a~%" "query" "rows" "median" "iter")
  (dolist (entry *movie-benchmark-queries*)
    (let ((name (car entry))
          (cql (cdr entry))
          (times nil)
          (rows 0))
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
        (format t "~&~48a ~8d ~9,2fms ~8d~%"
                name rows (%bench-median times) iterations))))

  (format t "~%~&Done.~%"))
