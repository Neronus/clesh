(asdf:defsystem #:shelisp
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :license "GPL 2 (see LICENSE)"
  :depends-on (#:trivial-shell
               #:lisp-unit)
  :components ((:file "shelisp"))
  :description ("Shelisp is a very short program that provides mechanisms for composing and running Unix shell (particularly bash) commands and constructs from Common Lisp.

Essentially, it provides a '!' syntax that you can use to run commands and a '[]' embedded mode where you can enter bash scripts and obtain the standard output as a lisp string, and some other features.

Lisp expressions can be included in any command or script using a '?' syntax.

Shelisp works on every implementation of Common Lisp that is supported
by trivial-shell."))