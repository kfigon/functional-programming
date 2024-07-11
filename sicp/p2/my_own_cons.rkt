#lang racket

; pair can be a function that returns a function to retrieve the values
(define (pair a b)
    (lambda (i) (if (= i 0) a b)))

(define (head p) (p 0))
(define (tail p) (p 1))

(let 
    ([a (pair 12 34)])
    (display (head a))
    (newline)
    (display (tail a)))
