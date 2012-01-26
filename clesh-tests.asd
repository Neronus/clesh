(asdf:defsystem #:clesh-tests
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :license "GPL 2 (see LICENSE)"
  :depends-on (#:clesh
               #:lisp-unit)
  :components ((:file "clesh-tests"))
  :description ("Unit tests for clesn

To run the tests, load this package and
evaluate (lisp-unit:run-all-tests clesh-tests)"))
