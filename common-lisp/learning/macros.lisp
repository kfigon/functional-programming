(defun endl () (format t "====~%"))

; code that will be expanded during compile time
; e.g. prime number generation at compile time or alter order of evaluation

(defmacro print-macro (x)
    (print x)) ; this will print during macro expansion
    ; `(print x) ; this would return code out of macro
    ; ` vs ' - first allows us to unsert evaluated values into expression

(print-macro (+ 2 3)) ; prints: (+ 2 3)
(print-macro ())
(print-macro `foobar) ; prints `foobar
(print-macro 'foobar) ; prints 'foobar
(endl)


(defmacro print-macro2 (x)
    `(print ,x)) ; this will return print code and evaluate x. otherwise x is just a symbol, not resolved. unbounded

(print-macro2 (+ 2 3)) ; prints 5
(print-macro2 ())
(print-macro2 "hello")
(print-macro2 `foobar) ; prints FOOBAR
(print-macro2 'foobar) ; prints FOOBAR
(endl)

(defmacro my-list (&rest items)
    `(list ,@items)) ; ,@ inserts the inside, but flattens. It splices it
                    ; (my-list 1 2 3) -> (list 1 2 3) vs
                    ; (my-list 1 2 3) -> (list (1 2 3))

(print (my-list 1 2 3))
(endl)

; progn coalesces many forms into 1. We can do multi line ifs with it
; executes multiple expressions sequentially, it's a block of code
;  rest vs body - body provided better ide support
(defmacro my-progn (&rest forms)
    `(progn ,@forms))

(print 
    (my-progn 
        (print "hello")
        (print "world")
        (+ 10 20))) ; evaluate last expr
(endl)

(defmacro my-when (condition &body body)
    `(if ,condition 
         (progn ,@body)))

(my-when t
    (print "print me")
    (print 'second)
    (print 'third))
(endl)

(my-when nil
    (print "this should not be")
    (print 'printed))
(endl)


; destructuring
(defmacro my-let ((variable value) &body body)
    `(let ((,variable ,value))
        ,@body))

(my-let (x 10)
    (print x)
    (print (+ x 1)))
(endl)

;gensym - when during macro expansion we can shadow something outside, we
; can generate unique name

; Use gensym for variables that your macro introduces internally 
; and that aren't intended to be visible to the caller.
(defmacro my-add-ten (expression)
    `(let ((tmp 10))
        (+ tmp ,expression)))

(print (my-add-ten 4))

(let ((tmp 100))
    (print (my-add-ten tmp))) ; this is 20, because tmp 100 is shadowed!

(defmacro my-add-ten2 (expression)
  (let ((tmp (gensym)))
    `(let ((,tmp 10))
        (+ ,tmp ,expression))))

(let ((tmp 100))
    (print (my-add-ten2 tmp)))  ; this now works, tmp is uniquely generated
(endl)