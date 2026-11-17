; this actually executes loop and splices result inside
; append vs collect - (a) (a) (a) vs ((a) (a) (a))
(defmacro repeat-it (cnt &body body)
    `(progn 
        ,@(loop repeat cnt append body)))


(defmacro show (form)
    `(format t "~a => ~a~%" ',form ,form))

(defmacro show-all (&body forms)
    `(progn
        ,@(loop for f in forms collect `(show ,f))))


(defmacro my-while ((var init-form) conditionz &body body)
  ; for initiaties the var all the time, var reuses the variable
    `(loop with ,var = ,init-form 
        while ,conditionz
        collect (progn ,@body)))

; (defvar f '(my-while (x 0) (< x 5) (print x) (incf x)))