; basic conditionals, looping, stdout, arithmetic

(defun endl () (format t "====~%"))
(defun pp (v) 
    (format t "~a~%" v)
    (endl))

(format t "~a~%" (+ 3 (* 4 5)))
(endl)

(defun avg (a b c)
    (let ((s (+ a b c)))
        (truncate s 3)))

(pp (avg 3 5 7))


(defun my-abs (v)
    (if (< v 0) 
        (* -1 v)
        v))

(format t "~a~%" (my-abs -4))
(format t "~a~%" (my-abs 4))
(format t "~a~%" (my-abs 0))
(endl)

(defun classify-number (v)
    (cond ((> v 0) :positive)
          ((< v 0) :negative)
          (t :zero)))

(format t "~a~%" (classify-number 4))
(format t "~a~%" (classify-number -4))
(format t "~a~%" (classify-number 0))
(endl)


(defun in-range-p (a start end)
    (and (>= a start) (<= a end)))

(format t "~a~%" (in-range-p 2 1 10))
(format t "~a~%" (in-range-p 0 1 10))
(format t "~a~%" (in-range-p 11 1 10))
(endl)

(defun calculate ()
    (let ((x 10)
          (y 3))
          (list (+ x y)
                (* x y)
                (- x y))))

(pp (calculate))


(defun swap-ends (v)
    (list (third v) (second v) (first v)))

(pp (swap-ends `(a b c)))

(defun make-symbol-greeting (name)
    (list :hello name))

(pp (make-symbol-greeting `alice))


