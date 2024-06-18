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


; 1.7
(define (improve-guess v guess)
    (define quotient (/ v guess))
    (/ (+ quotient guess) 2))

; get previous result
(define (good-enough2? prev guess)
    (< 
       (abs (- prev guess))
       0.001))

(define (sqrt-iter v guess)
   (define next (improve-guess v guess))
    (if
        (good-enough2? guess next) 
        next
        (sqrt-iter v (improve-guess v guess))))

(define (sqrt v) (sqrt-iter v 1.0))


(sqrt 0.00000000123)
(sqrt 0.0001)
(sqrt 2)
(sqrt 9)
(sqrt 100000000000000000)

; 1.8 - cube root
(define (cube-root v) (cube-iter v 1.0))

(define (good-enough3? x guess)
   (< 
      (abs (- x (* guess guess guess)))
      0.001))

(define (cube-iter x guess)
   (if
      (good-enough3? x guess)
      guess
      (cube-iter x (foo guess x))))

(define (foo guess x) 
   (/ (+ (/ x (* guess guess))
         (* 2 guess))
      3))


(printf "\n")

(cube-root 8) ; 2
(cube-root 27) ; 3