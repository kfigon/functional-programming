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


(defun safe-first (tab)
    (if (< (length tab) 1)
        nil 
        (first tab)))

(print (safe-first nil))
(print (safe-first ()))
(print (safe-first `()))
(print (safe-first `(1)))
(print (safe-first `(2 3)))
(endl)

(defun contains-p (v tab)
    (if (equal nil (member v tab))
        nil
        t))
(print (contains-p 'c '(a b c)))
(print (contains-p 'b '(a b c)))
(print (contains-p 'z '(a b c)))
(print (contains-p 'a '()))
(endl)

(defun remove-negatives (tab)
    (remove-if 
        (lambda (x) (< x 0))
        tab))

(print (remove-negatives `(-3 2 -1 5 0)))
(print (remove-negatives `(3 2 1 5 0)))
(print (remove-negatives `()))
(endl)

(defun count-even (tab)
    (count-if 
        (lambda (x) (= (mod x 2) 0))
        tab))

(print (count-even `(1 2 4 7 8)))
(endl)

(defun fizzbuzz (num)
  (loop for x from 1 to num
    do (print (cond 
        ((= (mod x 15) 0) "Fizzbuzz")
        ((= (mod x 3) 0)"Fizz")
        ((= (mod x 5) 0) "Buzz")
        (t (format nil "~A" x))))))

(fizzbuzz 30)
(endl)

(defun sum-list (xs)
    (let ((sum 0))
      (dolist (x xs)
        (setf sum (+ sum x)))
      sum))

(print (sum-list `(1 2 3 4)))
(print (sum-list nil))
(endl)

(defun range-list (num)
    (loop for x from 0 below num
        collect x))

(print (range-list 5))
(endl)

(defun my-max (xs)
    (let ((mx 0))
         (dolist (x xs)
            (when (> x mx)
                (setf mx x)))
          mx))
(print (my-max nil))
(print (my-max `(1 2 32 4 5)))
(endl)
