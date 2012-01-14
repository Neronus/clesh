(asdf:defsystem #:clesh
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :license "FreeBSD (see LICENSE)"
  :depends-on (#:trivial-shell)
  :components ((:file "clesh"))
  :description ("Clesh is a very short program that provides mechanisms for running and composing Unix shell commands and constructs from Common Lisp.

Essentially, it provides a '!' syntax that you can use to run commands and a '[]' embedded mode where you can enter bash scripts and obtain the standard output as a lisp string, and some other features.

Lisp expressions can be included in any command or script using a '?' syntax.

Clesh works on every implementation of Common Lisp that is supported
by trivial-shell."))