;;;; scalaxy.asd --- ASDF system definition for Scalaxy
;;;;
;;;; Scalaxy: a multi-purpose, cloud-ready distributed database.
;;;; Implementation language: Common Lisp.

(defsystem "scalaxy"
  :description "Scalaxy: multi-purpose cloud-ready distributed database."
  :version "1.6.7"
  :author "Artem Andreenko <miolini>"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "util")
                             (:file "protocol")
                             (:file "storage")
                             (:file "consistent-hash")
                             (:file "replication")
                             (:file "node")
                             (:file "tcp")
                             (:file "json")
                             (:file "http")
                             (:file "web")
                             (:file "gateway")
                             (:file "cluster")
                             (:file "api")
                             (:file "main")))))

(defsystem "scalaxy/tests"
  :description "Test suite for Scalaxy."
  :depends-on ("scalaxy")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "run-tests")))))
