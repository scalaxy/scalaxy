;;;; scripts/run-tests.lisp --- run the Scalaxy test suite and exit
;;;;
;;;; Usage: sbcl --script scripts/run-tests.lisp

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")
(asdf:load-system "scalaxy/tests")

#+sbcl
(sb-ext:exit :code (scalaxy-tests:run-all-tests))
#-sbcl
(uiop:quit (scalaxy-tests:run-all-tests))
