;;;; tests/package.lisp

(defpackage #:scalaxy-tests
  (:use #:cl #:scalaxy)
  (:shadow #:get #:delete)
  (:export #:run-all-tests))
