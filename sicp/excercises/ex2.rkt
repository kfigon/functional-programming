#lang racket

; 2.2
; segments on a plane
(define make-point cons)
(define get-x car)
(define get-y cdr)

(define make-segment cons)
(define get-start car)
(define get-end cdr)

(define (average a b) (/ (+ a b) 2))
(define (average-point a b) 
    (make-point (average (get-x a) (get-x b))
                (average (get-y a) (get-y b))))

(define (midpoint seg)
    (average-point (get-start seg) (get-end seg)))

(define (print-point a)
    (display "(")
    (display (get-x a))
    (display ",")
    (display (get-y a))
    (display ")")
    (newline))

(let ([a (make-point 1.0 2.0)]
      [b (make-point 18.0 4.0)])
      (print-point (midpoint (make-segment a b))))