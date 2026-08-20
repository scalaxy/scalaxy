;;;; scripts/test-live-bulk-import.lisp
;;;; Build the complete taxi graph locally, then write every authoritative
;;;; record through the live cluster's /api/bulk-keys endpoint.  This is an
;;;; end-to-end ingest test; no S3 bucket is populated directly.
(require :asdf)
(asdf:load-asd (truename "scalaxy.asd"))
(asdf:load-system "scalaxy")
(load "benchmarks/nyc-taxi/load.lisp")
(in-package #:scalaxy)

(defun %bulk-hex (v)
  (with-output-to-string (out)
    (loop for b across v do (format out "~2,'0X" b))))
(defun %bulk-post (endpoint db pairs chunk)
  (let* ((file (format nil "/tmp/scalaxy-bulk-~d.json" chunk))
         (body (json-encode
                (list (cons "records"
                            (mapcar (lambda (p)
                                      (list (cons "key" (car p))
                                            (cons "value" (%bulk-hex (cdr p)))))
                                    pairs))))))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string body out))
    (let ((result (uiop:run-program
                   (list "curl" "-fsS" "--max-time" "600"
                         "-X" "POST" "-H" "Content-Type: application/json"
                         "--data-binary" (format nil "@~a" file)
                         (format nil "~a/api/bulk-keys?db=~a" endpoint db))
                   :output :string :error-output :string)))
      (delete-file file)
      result)))

(let* ((endpoint (or (uiop:getenv "SCALAXY_HTTP") "http://127.0.0.1:8080"))
       (db (or (uiop:getenv "SCALAXY_IMPORT_DB") "live"))
       (directory (or (uiop:getenv "NYC_TAXI_DIR") "benchmarks/nyc-taxi/"))
       (chunk-size (parse-integer (or (uiop:getenv "SCALAXY_IMPORT_CHUNK") "5000")))
       (limit (let ((s (uiop:getenv "SCALAXY_IMPORT_LIMIT"))) (and s (plusp (length s)) (parse-integer s))))
       (store (make-store))
       (graph (make-local-graph store :db db))
       (checkpoint (or (uiop:getenv "SCALAXY_IMPORT_CHECKPOINT")
                       "/tmp/scalaxy-import.checkpoint"))
       (resume (if (probe-file checkpoint)
                   (parse-integer (string-trim '(#\Space #\Tab #\Newline)
                                               (uiop:read-file-string checkpoint))
                                  :junk-allowed t)
                   0))
       (seen 0) (chunk nil) (chunk-no (floor resume chunk-size)) (sent resume))
  (format t "building authoritative taxi records locally; resume=~d~%" resume)
  (multiple-value-bind (zones trips)
      (load-nyc-taxi graph (pathname directory) :mode :per-trip :limit limit)
    (format t "built zones=~d trips=~d; posting through ~a~%" zones trips endpoint)
    (uiop:run-program (list "curl" "-fsS" "--max-time" "30" "-X" "POST"
                            "-H" "Content-Type: application/json"
                            "-d" (format nil "{\"name\":\"~a\"}" db)
                            (format nil "~a/api/databases" endpoint))
                      :output :string)
    (g-map graph
           (lambda (p)
             (incf seen)
             (when (> seen resume)
               (push p chunk))
             (when (and (> seen resume) (>= (length chunk) chunk-size))
               (incf chunk-no)
               (let ((batch chunk) (n (length chunk)))
                 (%bulk-post endpoint db (nreverse batch) chunk-no)
                 (incf sent n)
                 (with-open-file (out checkpoint :direction :output :if-exists :supersede)
                   (format out "~d" sent)))
               (setf chunk nil)
               (when (zerop (mod chunk-no 10))
                 (format t "posted chunks=~d records=~d~%" chunk-no sent)
                 (finish-output)))))
    (when chunk
      (incf chunk-no)
      (let ((batch chunk) (n (length chunk)))
        (%bulk-post endpoint db (nreverse batch) chunk-no)
        (incf sent n)
        (with-open-file (out checkpoint :direction :output :if-exists :supersede)
          (format out "~d" sent)))
      )
    (format t "complete chunks=~d records=~d zones=~d trips=~d~%"
            chunk-no sent zones trips)))
