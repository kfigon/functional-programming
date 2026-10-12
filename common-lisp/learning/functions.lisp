(defun sum2 (a b)
    (+ a b))

(defun pp (v) (format t "~a~%" v))

(pp "just sum")
(pp (sum2 1 3))

; anything after this tag will get optional value. default values, 
; otherwise nil. Optional value can be next symbol, like value from a
; third value will be t/nil when c was provided/defaulted
(defun optional-params (a &optional (b 2) (c a c-supplied-p))
    (+ a b c))


(pp "optionals")
(pp (optional-params 1))
(pp (optional-params 1 2))
(pp (optional-params 1 2 3))

; var length params
(defun var-len (a &rest values)
    (apply #' + a values))

(pp "rest params")
(pp (var-len 1))
(pp (var-len 1 2))
(pp (var-len 1 2 3))
(pp (var-len 1 2 3 4))

; named params. nil if not provided, can be defualt or -p without additional
; params. Put &key after any required, optional, rest params
; keywords can also be renamed if needed
; (defun foo (&key ((:apple a)) (:box b) 0) (list a b))
(defun keyword-params (&key a b (c 3)) (+ a b c))

(pp "keywords")
(pp (keyword-params :a 1 :b 2 :c 3))
(pp (keyword-params :a 1 :b 2))

; order is: required, optionals, rest, keyword
; but usually it's just 
; required + optional + rest 
; required + keyword
; other are surprising

; Mixing Different Parameter Types
