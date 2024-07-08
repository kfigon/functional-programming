#lang racket

; square root using newtonian method
(define (improve-guess v guess)
    (define quotient (/ v guess))
    (/ (+ quotient guess) 2))

(define (good-enough? target guess)
    (define square (* guess guess))
    (< 
       (abs (- target square))
       0.001))

(define (sqrt-iter v guess)
    (if
        (good-enough? v guess) guess
        (sqrt-iter v (improve-guess v guess))))

(define (sqrt v) (sqrt-iter v 1.0))

; (sqrt 0.00000000123) ; wrong
(sqrt 0.0001)
(sqrt 2)
(sqrt 9)
; (sqrt 100000000000000000) ; to long


