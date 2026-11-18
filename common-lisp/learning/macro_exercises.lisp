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