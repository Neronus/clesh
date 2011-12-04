# SHELISP — A Unix shell interface for Common Lisp

Alexandru Dan Corlan, MD, PhD

Academy of Medical Sciences, University Hospital of Bucharest, Romania

    SHELISP — Unix shell interface with Common Lisp
    Version 2, August 20, 2006
    Address for Correspondence
    Dr. Alexandru Corlan,
    University Hospital of Bucharest, 169 Spl. Independenei, Bucharest 5,
    Cardiology Research Unit, floor X, room 41. http://dan.corlan.net

This manual documents SHELISP, a very short and simple program,
written in CommonLisp, that extends the CommonLisp syntax with
constructs similar to unix shells, by invoking such shells.

## Quick guide to shelisp

Load any Common Lisp implemenation, and on the REPL load shelisp by
invoking
    (load "shelisp.lisp")
###  The bang (!) escape to shell

Now you can say (the '`*`' is already put there by your CL):

    * !ls
    
And it will execute the shell ls command (by running a bash instance and passing the command to it.
Of course, you are actually in Lisp. You can try this:

    * (defun factorial (x) (if (zerop x) 1 (* x (factorial (1- x)))))
    FACTORIAL
    * (factorial 33)
    8683317618811886495518194401280000000
    
So, if you enter '`!`' the rest of the line (until the first end of line that is not escaped with a “\”) is interpreted
as a bash command and the result is printed on the standard output.
Now try:

    * !echo ?(+ 2 3) zuzu
    5zuzu
    
The '`?`' is the 'lisp escape'. It is followed by an s-expression which is read, executed and printed (with
princ) and the printed result replaces the '`?`' and the expression in the shell command. It can be any Lisp
expression.

    * !echo ?(+ 2/3 2/11) " <- this is a fraction"
    28/33 ;; <- this is a fraction
    * !echo ?(factorial 100) " <- this is a beegnum"
    933262154439441526816992388562667004907159682643816\
    21468592963895217599993229915608941463\
    97615651828625369792082722375825118521091686400\
    0000000000000000000000 <- this is a beegnum
    
Provided that you already entered the factorial definition above.
You may escape the '`?`' with a '`\`' to have it transfered to the shell command. for example:

    * !echo \?\(+ 2 3\)
    ?(+ 2 3)
    4
    

### Embedded bash scripts

Anything written between square brackets is interpreted as a shell script. What the script prints on the standard
output, however, is not displayed, but collected in a string and returned as a result of the bracketed expression.
For example:

    * [echo hi there!]
    "hi there!
    "


One thing that you can't ordinarily do in bash:

    * (dotimes (i 7) (princ [echo ?i ]) )
    0
    1
    2
    3
    4
    5
    6
    
You can now say:

    * (defun count-to (x) (dotimes (i x) (princ [echo ?i ]) ))
    COUNT-TO
    * (COUNT-TO 3)
    0
    1
    2

Or, for example:

    * (defun c-c-count-to (x)
        (dotimes (i x)
          (princ [echo ?i | sed 's/\(.\)/\1-\1-\1/' ]) ))
      C-C-COUNT-TO
    * (c-c-count-to 3)
      0-0-0
      1-1-1
      2-2-2
    
### Switching to shell mode (double-bang, '!!')

If you enter a double bang ('`!!`') then the prompter is changed to '`$`' and you can issue unescaped shell command
until you start a line with '`!!`' again - then you revert to 'lisp mode'.
Constructs with '`?`' are honored and are read and evaluated immediately by Lisp. Results of commands are
printed immediately after being issued.
For example:

    * !!
    $ ls
    Makefile
    shelisp.lisp
    shelisp_mn.aux
    shelisp_mn.log
    shelisp_mn.pdf
    shelisp_mn.tex
    shelisp_sc.aux
    shelisp_sc.log
    shelisp_sc.pdf
    shelisp_sc.tex
    shelisp.tex
    spec.txt
    $ # ?(setq bb 33.34)
    $ echo ?bb " is " ?(sqrt bb) " squared."
    33.34 is 5.77408 squared.
    $ echo ?bb " is " ?(sqrt bb) " squared." >somefile.txt
    $ cat somefile.txt
    33.34 is 5.77408 squared.
    $ echo "I am almost sure that " 'cat somefile.txt'
    I am almost sure that 33.34 is 5.77408 squared.
    $ !!
    $
    NIL
    *
    
Notice how purely Lisp commands, such as variable assignement (bindings), can be escaped with '`#`' char-acters as bash comments.

### Run scripts as Lisp calls

The function script takes as argument a string and executes it as a bash script, returning the standard output
of the script as a string.

    * (script "ls")
    "
    Makefile
    shelisp.lisp
    shelisp_mn.aux
    shelisp_mn.log
    shelisp_mn.pdf
    shelisp_mn.tex
    shelisp_sc.aux
    shelisp_sc.log
    shelisp_sc.pdf
    shelisp_sc.tex
    shelisp.tex
    spec.txt
    "
    
### Templates

A template is a string introduced with '`#[`' and ended with '`]#`'.
It is treated like an usual string, however
'`?`'-preceded lisp expressions are evaluated and their result printed inside the string.
For example:

    (defvar *title* "Title of an empty page")
        
    ...
    
    (prin1 #[Content-type: text/html
    <html>
    <head><title> ?*title* </title></head>
    <body></body>
    </html>
    ]#)
    
Will print to '`*standard-output*`':

    Content-type: text/html
    <html>
    <head><title>Title of an empty page</title></head>
    <body></body>
    </html>

### Storable templates

One problem with templates is that we might desire to run them at a later time, in a different context. For
example, we might want to define a variable with a generic web-page template and then generate actual web
pages at later times, with various contents.
We use '`#{`' and '`}#`' for this purpose. In the example below notice that each time the value of variable `A` is
evaluated, the `BB` in the evaluation context is used.

    *(setf bb 9)
    9
    * (setf a #{ plus: ?bb :sulp }#)
    (MIXED-TEMPLATE " plus: " BB " :sulp ")
    * (setf bb 10)
    10
    * (eval a)
    " plus: 10 :sulp "
    * (setf bb 22)
    22
    * (eval a)
    " plus: 22 :sulp "
    * (defun calc-a (bb) (eval a))
    CALC-A
    * (calc-a 88)
    " plus: 88 :sulp "
    * (eval a)
    " plus: 22 :sulp "

## Technical issues

Expressions preceded with '`?`' in the embedded shell scripts (with the '`[]`' syntax) are evaluated in the context
where they appear, at 'eval' time; expressions in the 'bang' context (with the '`!`' or the '`!!`' syntax) are evaluated
at read time, in the context of the last top level form before the form containing the bangs. This is because the
bangs are intended for shell-only commands, normally given at the top level (command line) with immediate
results. The embedded scripts are supposed to become part of functions or more complex forms, are parsed at
read time and prepared to be executed at runtime.

In the 'bang' forms only simple shell commands can be issued as the reader does not detect the circumstances
when a construct (such as ca '`case`') occupies more than one line. In the embedded form or with the script
command, any script can be executed.