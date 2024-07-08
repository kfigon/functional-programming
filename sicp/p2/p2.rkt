#lang racket

; library for ration numbers

(define (make-rational a b) (cons a b)) ; this could be enhanced to use lower terms. Sign could also be normalized
(define (num rat) (car rat))
(define (denom rat) (cdr rat))

; more performant and clever alternatives
; (define make-rational cons)
; (define num car)
; (define denom cdr)

; ax/ay + bx/by = ((axby) + (bxay))/(ay*by)
(define (add-rat a b) 
    (make-rational 
        (+ (* (num a) (denom b))
           (* (num b) (denom a)))
        (* (denom a) (denom b))))

(define (sub-rat a b) 
    (make-rational 
        (- (* (num a) (denom b))
           (* (num b) (denom a)))
        (* (denom a) (denom b))))

(define (mult-rat a b) 
    (make-rational 
                    (* (num a) (num b))
                    (* (denom a) (denom b))))

(define (eq-rat a b) 
    (= (* (num a) (denom b)) 
       (* (num b) (denom a))))


(define (print-rat x)
    (newline)
    (display (num x))
    (display "/")
    (display (denom x)))

(print-rat (add-rat (make-rational 1 2) (make-rational 3 4)))
(print-rat (add-rat (make-rational 1 3) (make-rational 1 3)))