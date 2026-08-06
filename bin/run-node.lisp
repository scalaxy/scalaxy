;;;; bin/run-node.lisp --- bootstrap a standalone Scalaxy node

(require :asdf)
(asdf:load-asd (merge-pathnames "../scalaxy.asd" *load-truename*))
(asdf:load-system "scalaxy")

#+sbcl
(apply #'scalaxy:main (cdr sb-ext:*posix-argv*))
