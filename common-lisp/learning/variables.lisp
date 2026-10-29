(defun endl () (format t "====~%"))
(let ((x 10)
    (y 20) 
    z) ; z is nil
    (format t "~a~%" (+ x y)))

(endl)
(dotimes (x 5)
    (format t "~a~%" x))

(endl)

; normal let cant use previous values inside following assignments. Need to use nested lets
(let* ((x 5)
      (y (+ x 1)))
      (format t "~d~%" (+ x y)))
(endl)

; defvar defparameter for global values
; defconstant for constants

; defparam - always assigns initial val
; defvar assigns when variable is undefined. it can also define without initial val

; with let we can also scope global values, by temp overriding them
; this way stdout is temporarily replaced by file, so we can log to file, then
; drop the scope. temp shadow global dynamically. all changes affect the shadowed val
; (let ((*STANDARD-OUTPUT* *some-other-stream-like-a-file*))
;     (do-things))


; assignment
(let ((x 10))
    (format t "~a~%" x)
    (setf x 123) ; (setf x 10 y 20) for multi assingments
    (format t "~a~%" x))

; setting array or special place in struct - with specifying the reference target
; Simple variable:    (setf x 10) 
; Array:              (setf (aref a 0) 10)
; Hash table:         (setf (gethash 'key hash) 10)
; Slot named 'field': (setf (field o) 10)



