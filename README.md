# CLESH — A Unix shell interface for Common Lisp

Christian von Essen
    
This manual documents CLESH (Common Lisp Embedded Shell) , a very
short and simple program, written in Common Lisp, that extends Common
Lisp to embed shell code in a manner similar to perl's backtick. It
has been forked from from SHELISP by Alexandru Dan Corlana

## Quick guide to clesh

Load any Common Lisp implemenation, and on the REPL load clesh by
invoking

    (load "clesh.lisp")
    (use-package 'named-readtables)
    (in-readtable clesh:syntax)

### The bang (!) escape to shell

For convenience, and for use from the REPL, you can emit single line
calls to the shell using the bang.

You can say (the '`*`' is already put there by your CL):

    * !ls
    
And it will execute the shell ls command (by running a bash instance
and passing the command to it. Of course, you are actually in Lisp.
You can try this:

    * (defun factorial (x) (if (zerop x) 1 (* x (factorial (1- x)))))
    FACTORIAL
    * (factorial 33) 8683317618811886495518194401280000000
    
So, if you enter '`!`' the rest of the line (until the first end of
line that is not escaped with a “\”) is interpreted as a bash command
and the result is printed on the standard output. Now try:

    * !echo ?(+ 2 3) zuzu 5 zuzu
    
The '`?`' is the 'lisp escape'. It is followed by an s-expression
which is read, executed and printed (with princ) and the printed
result replaces the '`?`' and the expression in the shell command. It
can be any Lisp expression.

    * !echo ?(+ 2/3 2/11) " <- this is a fraction" 28/33 <- this is a
    fraction
    * !echo ?(factorial 100) " <- this is a beegnum"
    933262154439441526816992388562667004907159682643816\
    21468592963895217599993229915608941463\
    97615651828625369792082722375825118521091686400\
    0000000000000000000000 <- this is a beegnum
    
Provided that you already entered the factorial definition above. You
may escape the '`?`' with a '`\`' to have it transfered to the shell
command. for example:

    * !echo \?\(+ 2 3\) ?(+ 2 3) 4

You can also use the escape for multiple lines. Just escape the
newline with a backslash. The newlines will then be ignored, like
this:

   *!echo foo\ bar\ baz

will output `foo bar baz`.
    
### Embedded shell scripts

Anything written between square brackets is interpreted as a shell
script. What the script prints on the standard output, however, is not
displayed, but collected in a string and returned as a result of the
bracketed expression. For example:

    * [echo hi there!] "hi there! " "" 0


One thing that you can't ordinarily do in bash:

    * (dotimes (i 7) (princ [echo ?i ])) 0 1 2 3 4 5 6
    
You can now say:

    * (defun count-to (x) (dotimes (i x) (princ [echo ?i ]))) COUNT-TO
    * (COUNT-TO 3) 0 1 2

Or, for example:

    * (defun c-c-count-to (x) (dotimes (i x) (princ
        [echo ?i | sed 's/\(.\)/\1-\1-\1/' ]) )) C-C-COUNT-TO
    * (c-c-count-to 3) 0-0-0 1-1-1 2-2-2
      
### Switching to shell mode (double-bang, '!!')

If you enter a double bang ('`!!`') then the prompter is changed to
'`$`' and you can issue unescaped shell command until you start a line
with '`!!`' again - then you revert to 'lisp mode'. Constructs with
'`?`' are honored and are read and evaluated immediately by Lisp.
Results of commands are printed immediately after being issued. For
example:

    * !! $ ls Makefile clesh.lisp clesh_mn.aux clesh_mn.log
    clesh_mn.pdf clesh_mn.tex clesh_sc.aux clesh_sc.log clesh_sc.pdf
    clesh_sc.tex clesh.tex spec.txt $ # ?(setq bb 33.34) $ echo ?bb "
    is " ?(sqrt bb) " squared." 33.34 is 5.77408 squared. $ echo ?bb "
    is " ?(sqrt bb) " squared." >somefile.txt $ cat somefile.txt 33.34
    is 5.77408 squared. $ echo "I am almost sure that " 'cat
    somefile.txt' I am almost sure that 33.34 is 5.77408 squared. $ !!
    $ NIL *
    
Notice how purely Lisp commands, such as variable assignement
(bindings), can be escaped with '`#`' char-acters as bash comments.

### Run scripts as Lisp calls

The function script takes as argument a string and executes it as a
bash script, returning the standard output of the script as a string
as first value, the error output of the script as second value and the
return value as last value.

    * (script "ls") " Makefile clesh.lisp clesh_mn.aux clesh_mn.log
    clesh_mn.pdf clesh_mn.tex clesh_sc.aux clesh_sc.log clesh_sc.pdf
    clesh_sc.tex clesh.tex spec.txt " "" 0
    
### Templates

A template is a string introduced with '`#[`' and ended with '`]#`'.
It is treated like an usual string, however '`?`'-preceded lisp
expressions are evaluated and their result printed inside the string.
For example:

    (defvar *title* "Title of an empty page")
        
    ...
    
    (prin1
    #[Content-type: text/html <html> <head><title> ?*title* </title></head> <body></body> </html> ]#)
    
Will print to '`*standard-output*`':

    Content-type: text/html <html> <head><title>Title of an empty
    page</title></head> <body></body> </html>

### Storable templates

One problem with templates is that we might desire to run them at a
later time, in a different context. For example, we might want to
define a variable with a generic web-page template and then generate
actual web pages at later times, with various contents. We use '`#{`'
and '`}#`' for this purpose. In the example below notice that each
time the value of variable `A` is evaluated, the `BB` in the
evaluation context is used.

    *(setf bb 9) 9
    * (setf a #{ plus: ?bb :sulp }#) (MIXED-TEMPLATE " plus: " BB "
    :sulp ")
    * (setf bb 10) 10
    * (eval a) " plus: 10 :sulp "
    * (setf bb 22) 22
    * (eval a) " plus: 22 :sulp "
    * (defun calc-a (bb) (eval a)) CALC-A
    * (calc-a 88) " plus: 88 :sulp "
    * (eval a) " plus: 22 :sulp "

### Other fun things to do

The program that is called for the embedded script is controlled by
the variable `*shell*` in package `clesh`. If you want to use another
shell (or indeed anything else), then set this variable to the path of
the program to use.

For example, you can set it to `cat -n`, and you will get everything
you enclose in brackets back with line numbers in front:

      [ foo bar baz ]

will return as first string
         
     1 foo 2 bar 3 baz

Furthermore, this easily allows to talk to remote PCs via an ssh
connection or via `netcat`.
     
## Technical issues

Expressions preceded with '`?`' in the embedded shell scripts (with
the '`[]`' syntax) are evaluated in the context where they appear, at
'eval' time; expressions in the 'bang' context (with the '`!`' or the
'`!!`' syntax) are evaluated at read time, in the context of the last
top level form before the form containing the bangs. This is because
the bangs are intended for shell-only commands, normally given at the
top level (command line) with immediate results. The embedded scripts
are supposed to become part of functions or more complex forms, are
parsed at read time and prepared to be executed at runtime.

In the 'bang' forms only simple shell commands can be issued as the
reader does not detect the circumstances when a construct (such as ca
'`case`') occupies more than one line. In the embedded form or with
the script command, any script can be executed.

When interpolating a symbol, the reader might consider the closing `]`
or `}` part of that symbol. For example, [echo ?sl:*shell* ] will make
the reader complain about not finding the end of what it has to read.
Instead, you have to write `#[echo ?sl:*shell* ]#` (or
`#[ echo ?sl:*shell* ]#`). Note though, that this introduces an extra
space:

     * #[echo sl:*shell ]# "echo /bin/sh "

Another way to circumvent this problem is to write
`#[echo ?(values sl:*shell*)]#` instead.
