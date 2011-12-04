(asdf:defsystem #:shelisp-tests
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :license "GPL 2 (see LICENSE)"
  :depends-on (#:shelisp
               #:lisp-unit)
  :components ((:file "shelisp-tests"))
  :description ("Unit tests for shelisp.

To run the tests, load this package and
evaluate (lisp-unit:run-all-tests shelisp)"))