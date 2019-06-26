(defpackage #:clesh-tests
  (:use #:cl
        #:clesh
        #:lisp-unit)
  (:import-from #:clesh
                #:read-interpolated-string))

(in-package #:clesh-tests)

(define-test read-interpolated-string
  (assert-equal
   '("asd foo bar") (read-interpolated-string (make-string-input-stream "asd foo bar]") #\]))
  (assert-equal
   '("asd foo bar ]") (read-interpolated-string (make-string-input-stream "asd foo bar \\]]") #\]))
  (assert-equal
   '("asd foo bar " (+ 2 2) " ") (read-interpolated-string (make-string-input-stream "asd foo bar ?(+ 2 2) ]") #\]))
  (assert-equal
   '("asd foo bar " "4" " ") (read-interpolated-string (make-string-input-stream "asd foo bar ?(+ 2 2) ]") #\] nil t))
  (assert-equal
   '("asd foo") (read-interpolated-string (make-string-input-stream "asd foo]#") #\# #\]))
  (assert-equal
   '("asd foo]#") (read-interpolated-string (make-string-input-stream "asd foo\\]#]#") #\# #\]))
  (assert-equal
   '("asd foo]\\#") (read-interpolated-string (make-string-input-stream "asd foo]\\#]#") #\# #\]))
  (assert-equal
   '("asd ?(+ 2 2)") (read-interpolated-string (make-string-input-stream "asd \\?(+ 2 2)]") #\]))
  (assert-equal
   '("asd\\#") (read-interpolated-string (make-string-input-stream "asd\\#]") #\]))
  (assert-equal
   '("asd foo " (+ 2 2) " bar " (+ 3 3))
   (read-interpolated-string (make-string-input-stream "asd foo ?(+ 2 2) bar ?(+ 3 3)]") #\]))
  (assert-equal
   '("asd foo \\]#")
   (read-interpolated-string (make-string-input-stream "asd foo \\\\\\]#]#") #\# #\]))
  (assert-equal
   '("asd foo \\\\")
   (read-interpolated-string (make-string-input-stream "asd foo \\\\\\\\]#]#") #\# #\]))  
  (assert-equal
   '("asd foo \\\\]#")
   (read-interpolated-string (make-string-input-stream "asd foo \\\\\\\\\\]#]#") #\# #\]))
  (assert-equal
  '("echo foo")
  (read-interpolated-string
   (make-string-input-stream "echo foo]")
   #\]))
  (assert-equal
  '("echo foo")
  (read-interpolated-string
   (make-string-input-stream "echo foo}")
   #\}))
  (assert-equal
  '("echo foo")
  (read-interpolated-string
   (make-string-input-stream "echo foo}#")
   #\# #\}))
  (assert-equal
  '("echo foo")
  (read-interpolated-string
   (make-string-input-stream "echo foo]#")
   #\# #\]))
  (assert-equal
   '("echo ?(+ 2 2)")
   (read-interpolated-string
    (make-string-input-stream "echo \\?(+ 2 2)]")
    #\])))