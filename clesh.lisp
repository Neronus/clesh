;;;; Clesh: unix shell interface for CommonLisp
;;;; Copyright (c) 2003-2006 Alexandru Dan Corlan MD PhD (http://dan.corlan.net)
;;;; Copyright (c) 2011 Christian von Essen <christian@mvonessen.de>

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
;;;; v1.0:    January 14th, 2012
;;;;          Renamed to CLESh (Common Lisp Embedded Shell)

(defpackage #:clesh
  (:use #:cl
        #:trivial-shell
        #:named-readtables)
  (:nicknames #:clsh)
  (:export #:lines-to-list
           #:script
           #:*shell*
           #:syntax))

(in-package #:clesh)

(defparameter *shell* "/bin/sh"
  "Program to use to execute shell commands.")

(defun script (str &key (program *shell*))
  "Execute the STR string as a standard input of the program.

Returns three values.
   1. Standard output of the program
   2. Standard error of the program
   3. Exit code of the program"
  (shell-command (format nil "exec ~A" program) :input str))

(defun mixed-template (&rest strlist)
  "Concatenate list of arguments into a string.

Turns any argument that is not a string already into string using format's ~A."
  (let ((evs (apply #'concatenate 'string
                    (mapcar #'(lambda (x)
                                (format nil "~A" x))
                            strlist))))
    evs))

(defun mixed-script (&rest strlist)
  "Concatenate arguments like MIXED-TEMPLATE, execute result like SCRIPT.

Returns three values.
  1. Standard output of the program
  2. Standard error of the program
  3. Exit code of the program"
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

;; \?foo --> ?foo
;; \\?foo -->\<foo>
;; \\\?foo --> \?foo
;; \\\\?foo --> \\<foo>
;; \\\\\?foo --> \\?foo

(defun read-escapes (str end-char1 buffer)
  "Read escapes, push the appropriate number of escapes onto
the buffer. If the last character is special and not escaped,
then this character is returned. Otherwise we return nil."
  (loop :for escapes :from 1
        :for char = (read-char str)
        :while (char= char #\\)
        :finally
          (let ((is-special (member char (list end-char1 #\?) :test #'char=)))
            (if is-special
                (multiple-value-bind (div rem) (floor escapes 2)
                  (dotimes (i div)
                    (vector-push #\\ buffer))
                  (if (= rem 0)
                      (return char)
                      (vector-push char buffer)))
                (progn
                 (dotimes (i escapes)
                   (vector-push #\\ buffer))
                 (vector-push char buffer))))))

(defun read-interpolated-string (str end-char1 &optional end-char2 eval-at-read)
  "Read from a stream until a delimiter is found and interpolate.

The delimiter is
 - the character END-CHAR1 if END-CHAR2 is nil
 - the sequence END-CHAR2 END-CHAR1 if END-CHAR2 is not nil.

Interpolation starts with ?, and the next form (i.e., lisp form)
is interpolated. If EVAL-AT-READ is not NIL, then the form will
be evaluated and converted into a string immediately.
Otherwise the form will be returned as is.

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
       :for last-char = nil :then char
       :for char = (read-char str)
       :with mixl = nil
       :if (char= char #\\)
       :do (let ((spec-char
                  (read-escapes str (or end-char2 end-char1) buffer)))
             (when spec-char (unread-char spec-char str)))
       :else :do (cond
             ((and (eql char end-char1) (or (null end-char2) (eql last-char end-char2)))
              (when end-char2
                (vector-pop buffer))
              (return-from read-interpolated-string
                (nreverse
                 (if (zerop (length buffer))
                     mixl
                     (cons buffer mixl)))))
             ((eql char #\?)
              (let ((form (read-preserving-whitespace str)))
                (push buffer mixl)
                (push (if eval-at-read
                          (format nil "~A" (eval form))
                          form) mixl)
                (setf buffer (get-buffer))))
             ((eql char #\\)
              (error "Should never happen"))
             (t
              (vector-push char buffer)))
       :when (buffer-full-p buffer)
       :do   (increase-buffer buffer))))

(defun enter-shell-mode (stream)
  "Read and execute successive shell commands, with any
   lisp expressions embedded. Expressions are evaluated at
   read time, as soon as a line is delivered. Implements the !! reader macro."
  (do () (nil)
    (princ "$ " *standard-output*)
    (let ((ll (apply #'concatenate 'string (read-interpolated-string stream #\Newline nil t))))
      (when (and (> (length ll) 1) (string= (subseq ll 0 2) "!!"))
        (return-from enter-shell-mode))
      (princ (script ll)))))

(defun the-only (list)
  "Returns the first element of a list.

Throws an error if the list does not contain exactly one argument."
  (if (or (endp list) (not (endp (cdr list))))
      (error "~A has not exactly one element." list)
      (car list)))

(defun simple-shell-escape-reader (stream char)
  (declare (ignore char))
  (let* ((ll
          (delete #\Newline
                  (the-only (read-interpolated-string stream #\Newline nil t)))))
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

(defreadtable clesh:syntax
  (:merge :standard)
  (:macro-char #\! #'simple-shell-escape-reader nil)
  (:macro-char #\[ #'embedded-shell-escape-reader nil)
  ;; Ignore closing brackets when reading them
  (:macro-char #\] #'(lambda (stream char)
                       (declare (ignore stream char))
                       (values)))
  (:macro-char #\} #'(lambda (stream char)
                       (declare (ignore stream char))
                       (values)))
  (:dispatch-macro-char #\# #\[ #'template-escape-reader)
  (:dispatch-macro-char #\# #\{ #'storable-template-escape-reader))

