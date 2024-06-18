#lang racket

; 1.2
(define some-v 
    (/ (+ 5 
          4 
          (- 2 
             (- 3 
                (+ 6 4/5))))
        (* 3 
           (- 6 2) 
           (- 2 7))))

; eval variable
some-v

; 1.3 sum of squares of 2 largest numbers
(define (square x) (* x x))
(define (sum-square a b) (+ (square a) (square b)))

(define (square-largest a b c)
   (cond 
      ((and (<= c a) (<= c b)) (sum-square a b))
      ((and (<= a b) (<= a c)) (sum-square b c))
      (else (sum-square a c))))

(and 
   (= (square-largest 10 10 10) (sum-square 10 10))
   (= (square-largest 1 10 10) (sum-square 10 10))
   (= (square-largest 10 1 10) (sum-square 10 10))
   (= (square-largest 10 10 1) (sum-square 10 10))
   (= (square-largest 1 10 100) (sum-square 10 100))
   (= (square-largest 1 100 10) (sum-square 10 100))
   (= (square-largest 10 1 100) (sum-square 10 100))
   (= (square-largest 10 100 1) (sum-square 10 100))
   (= (square-largest 100 10 1) (sum-square 10 100))
   (= (square-largest 100 1 10) (sum-square 10 100))
)

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; result of if -> function + or -


; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

; p is recursive. WHen this is called on  applicative-order evaluation  interpreter, it's infinite loop
; when it's normal (lazy eval), it's called just fine
; (test 0 (p))


; 1.6
; infinite loop. if is lazy and does not evaluate the expression that got false
; Since new-if is a function, and not a special form, each parameter subexpression will be evaluated before the procedure is applied