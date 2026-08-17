;;;; tests/package.lisp

(defpackage #:scalaxy-tests
  (:use #:cl #:scalaxy)
  (:shadowing-import-from #:scalaxy #:get #:delete)
  (:export #:run-all-tests #:run-tck))
