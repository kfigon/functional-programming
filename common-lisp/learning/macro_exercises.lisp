(defmacro twice (form)
    (list 'progn form form))
;equivalents, quote and unqoute (not eval)
(defmacro twice (form)
    `(progn ,form ,form))


; just generate symbols
(loop for i from 1 to 4 collect (intern (format nil "var-~a" i)))

(defvar vars '(10 20 30))
(loop 
    for var in vars
    for i from 1
    collect 
    ; quote outer, but unquote (eval) intermediate expressions
        `(,(intern (format nil "var-~a" i)) ,var))

(let* ((bindings 
    (loop 
        for var in vars
        for i from 1
        collect `(,(intern (format nil "var-~a" i)) ,var))))
    `(let (,@bindings) 
        (+ ,@(mapcar #'first bindings))))

(defmacro generate-sum (vars)
    (let* ((bindings 
        (loop 
            for var in vars
            for i from 1
            collect `(,(intern (format nil "var-~a" i)) ,var))))
        `(let (,@bindings) 
            (+ ,@(mapcar #'first bindings)))))


(defmacro my-unless (conditionz &body body)
    `(if (not ,conditionz)
         (progn ,@body)))

(defmacro infix+ (a b) `(+ ,a ,b))

(defmacro dbg-print (form) `(format t "~a => ~a~%" ',form ,form))

(defmacro time-exec (&body body)
    (let ((start (gensym)))
        `(let ((,start (get-internal-real-time)))
            (progn ,@body)
            (- (get-internal-real-time) ,start))))

; level 2

; cnt has to be known at compile time, this is a literal, not expression.
; that's why we don't eval ,cnt
(defmacro repeat-times (cnt &body body)
    `(progn ,@(loop repeat cnt append body)))

(defmacro set-if (condition val assignment)
    `(when ,condition 
           (setf ,val ,assignment)))

(defmacro swap (a b)
    (let ((temp (gensym)))
        `(let ((,temp ,a))
           (progn 
            (setf ,a ,b)
            (setf ,b ,temp)))))

(defmacro with-file ((stream file) &body body)
  ; no need for gensym, with-open-file handles lexical scoping
    `(with-open-file (,stream ,file :direction :output :if-exists :supersede)
       (progn ,@body)))

; level 3