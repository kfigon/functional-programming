#lang racket

; library for ration numbers

(define (make-rational a b) (cons a b)) 
(define (num rat) (car rat))
(define (denum rat) (cdr rat))

; more performant and clever alternatives
; (define make-rational cons)
; (define num car)
; (define denum cdr)

; ax/ay + bx/by = ((axby) + (bxay))/(ay*by)
(define (add-rat a b) 
    (make-rational 
        (+ (* (num a) (denum b))
           (* (num b) (denum a)))
        (* (denum a) (denum b))))

(define (sub-rat a b) 
    (make-rational 
        (- (* (num a) (denum b))
           (* (num b) (denum a)))
        (* (denum a) (denum b))))

(define (mult-rat a b) 
    (make-rational 
                    (* (num a) (num b))
                    (* (denum a) (denum b))))

(define (eq-rat a b) 
    (= (* (num a) (denum b)) 
       (* (num b) (denum a))))

