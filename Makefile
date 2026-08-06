SBCL ?= sbcl

.PHONY: test build clean

test:
	$(SBCL) --script scripts/run-tests.lisp

build:
	$(SBCL) --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '(asdf:load-asd (truename "scalaxy.asd"))' \
	  --eval '(asdf:load-system "scalaxy")'

clean:
	rm -rf .cache *.fasl src/*.fasl tests/*.fasl scripts/*.fasl scalaxy-data
