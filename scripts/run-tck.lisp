;;;; scripts/run-tck.lisp --- run the full openCypher TCK corpus
;;;;
;;;; Usage: sbcl --script scripts/run-tck.lisp [feature-dir]
;;;; Prints per-kind statistics and writes specs/tck-results.json.

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")
(asdf:load-system "scalaxy/tests")
(load (merge-pathnames "../tests/tck.lisp" *load-truename*))

(let* ((dir (or (second sb-ext:*posix-argv*) "specs/openCypher/tck/features"))
       (stats (scalaxy-tests:run-tck dir)))
  (format t "~&TCK corpus: ~a~%" dir)
  (format t "  total:       ~d~%" (getf stats :total))
  (format t "  pass:        ~d~%" (getf stats :pass))
  (format t "  fail:        ~d~%" (getf stats :fail))
  (format t "  unsupported: ~d~%" (getf stats :unsupported))
  (when (getf stats :fail)
    (format t "~%failures:~%")
    (dolist (f (getf stats :failures))
      (format t "  ~a :: ~a~%" (car f) (cadr f))))
  (let ((reasons (getf stats :reasons)))
    (when reasons
      (format t "~%unsupported by reason:~%")
      (maphash (lambda (k v) (format t "  ~4d ~a~%" v k)) reasons)))
  #+sbcl (sb-ext:exit :code (if (zerop (getf stats :fail)) 0 1))
  #-sbcl (uiop:quit (if (zerop (getf stats :fail)) 0 1)))
