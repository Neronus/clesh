;;;; shelisp: unix shell interface for CommonLisp
;;;; Copyright (c) 2003-2006 Alexandru Dan Corlan MD PhD (http://dan.corlan.net)
;;;; 

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License version 2, as published by
;;;; the Free Software Foundation.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;;; HISTORY
;;;; created: july 4, 2006
;;;; v2:      august 20, 2006
;;;;          #[ ]# syntax for template strings; eval expressions cand
;;;;          be added with ?expr
;;;;          read-preserving-whitespace now used in '?', cleaner results
;;;;          wrappers for: tex gs ls pwd mktemp dirname basename md5sum
;;;;          recursive version of md5sum that also works for directories
;;;; v2.1:    august 14, 2007
;;;;          sbcl compatibility
;;;;
;;;; v2.2:    December 4, 2011
;;;;          Made lines-to-list significantly faster (use loop instead of recursion)
;;;;          Removed several occurences of the dangling paranthesis
;;;;          Created a special variable *shell* that gets executed in the
;;;;            script command
;;;;          Use trivial-shell
;;;;          Unify all read functions into one
;;;;          Remove all bindings to shell programs
;;;;
;;;;  Known problems:
;;;;    * #[ My standard output: ?*standard-output*]#
;;;;      The reader considers the "]" to be part of the string it has to read.
;;;;      We cannot simply write
;;;;      #[ My standard output: ?*standard-output* ]#
;;;;      instead because then we will have a space in the end of the returned string.

(defpackage shelisp
  (:use cl trivial-shell lisp-unit)
  (:nicknames sl)
  (:export lines-to-list script *shell* enable script))

(in-package shelisp)

(defvar *shelisp-temp-directory* "/tmp/"
  "Directory where all temporary files are created.")

(defparameter *shell* "/bin/sh"
  "Program to use to execute shell commands.")

(defun script (str &key (program *shell*))
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (shell-command program :input str))

(defun mixed-template (&rest strlist)
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (let ((evs (apply #'concatenate 'string
                    (mapcar #'(lambda (x)
                                (format nil "~A" x))
                            strlist))))
    evs))

(defun mixed-script (&rest strlist)
  "Execute the string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (script (apply #'mixed-template strlist)))

(defun lines-to-list (text)
  "Transform the string TEXT into a list of strings, each representing
   on line of TEXT. This is suitable to postprocessing the standard output
   of many Unix commands (such as find or df) that return one result
   per line."
  (loop
     :with from-ptr = 0
     :for to-ptr :from 0 :below (length text)
     :when (char= (elt text to-ptr) #\newline)
     :collect
     (prog1
         (subseq text from-ptr to-ptr)
       (setf from-ptr (1+ to-ptr)))))

(defun read-interpolated-string (str end-char1 &optional end-char2 eval-at-read)
  "Read from a stream until a delimiter is found and interpolate.

The delimiter is
 - the character END-CHAR1 if END-CHAR2 is nil
 - the sequence END-CHAR2 END-CHAR1 if END-CHAR2 is not nil.

Interpolation starts with ?, and the next form (i.e., lisp form)
is interpolated. If EVAL-AT-READ is not NIL, then the form will
be evaluated and converted into a string immediately.
Otherwise the form will be return as is.

Returns a list. In this list, normal strings and interpolations alternate.
For example the string \"asd foo ?(+ 2 2) bar ?(+ 3 3)\"
will be read as (\"asd foo \" (+ 2 2) \" bar \" (+ 3 3))."
  (flet ((get-buffer ()
           (make-array 128 :element-type 'character :adjustable t :fill-pointer 0))
         (increase-buffer (buffer)
           (adjust-array buffer (* 2 (length buffer))))
         (buffer-full-p (buffer)
           (= (array-dimension buffer 0) (length buffer))))
    (loop
       :with buffer = (get-buffer)
       :for before-last-char = nil :then last-char
       :for last-char = nil :then char
       :for was-escaped = (and last-char (char= last-char #\\))
       :for before-was-escaped = (and before-last-char (char= before-last-char #\\))
       :for char = (read-char str)
       :with mixl = nil
       :do (cond
             ((and (eql char end-char1) (or (null end-char2) (eql last-char end-char2)))
              (if (or (and (null end-char2) was-escaped)
                      (and end-char2 before-was-escaped))
                  (vector-push char buffer)
                  (progn
                    (when end-char2
                      (vector-pop buffer))
                    (return-from read-interpolated-string
                      (nreverse
                       (if (zerop (length buffer))
                           mixl
                           (cons buffer mixl)))))))
             ((eql char #\?)
              (if was-escaped
                  (vector-push char buffer)
                  (let ((form (read-preserving-whitespace str)))
                    (push buffer mixl)
                    (push (if eval-at-read
                              (format nil "~A" (eval form))
                              form) mixl)
                    (setf buffer (get-buffer)))))
             ((eql char #\\)
              (when was-escaped
                (vector-push char buffer)))
             (t
              (vector-push char buffer)))
       :when (buffer-full-p buffer)
       :do   (increase-buffer buffer))))

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
   '("asd foo]#") (read-interpolated-string (make-string-input-stream "asd foo]\\#]#") #\# #\]))
  (assert-equal
   '("asd ?(+ 2 2)") (read-interpolated-string (make-string-input-stream "asd \\?(+ 2 2)]") #\]))
  (assert-equal
   '("asd#") (read-interpolated-string (make-string-input-stream "asd\\#]") #\]))
  (assert-equal
   '("asd foo " (+ 2 2) " bar " (+ 3 3))
   (read-interpolated-string (make-string-input-stream "asd foo ?(+ 2 2) bar ?(+ 3 3)]") #\])))

(defun enter-shell-mode (stream)
  "Read and execute successive shell commands, with eventual
   lisp expressions embedded. Expressions are evaluated at
   read time, as soon as a line is delivered. Implements the !! macro."
  (do () (nil)
    (princ "$ " *standard-output*)
    (let ((ll (apply #'concatenate 'string (read-interpolated-string stream #\Newline nil t))))
      (when (and (> (length ll) 1) (string= (subseq ll 0 2) "!!"))
        (return-from enter-shell-mode))
      (princ (script ll)))))

(defun simple-shell-escape-reader (stream char)
  (declare (ignore char))
  (let ((ll (apply #'concatenate 'string (read-interpolated-string stream #\Newline nil t))))
    (when (and (> (length ll) 0) (string= (subseq ll 0 1) "!"))
      (enter-shell-mode stream)
      (return-from simple-shell-escape-reader))
    (princ (script ll)))
  nil)

(defun embedded-shell-escape-reader (stream char)
  (declare (ignore char))
  (cons 'mixed-script (read-interpolated-string stream #\] )))

(defun template-escape-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (cons 'mixed-template (read-interpolated-string stream #\# #\])))

(defun storable-template-escape-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (list 'quote (cons 'mixed-template (read-interpolated-string stream #\# #\}))))

(defun enable (&optional (copy-readtable t))
  (when copy-readtable
    (setf *readtable* (copy-readtable)))
  (set-macro-character #\! #'simple-shell-escape-reader nil)
  (set-macro-character #\[ #'embedded-shell-escape-reader nil)
  (set-dispatch-macro-character #\# #\[ #'template-escape-reader)
  (set-dispatch-macro-character #\# #\{ #'storable-template-escape-reader))