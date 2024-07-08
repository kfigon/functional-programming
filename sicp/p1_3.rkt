#lang racket

(define (sum-it a b)
    (if 
        (> a b) 
        0
        (+ a (sum-it (+ a 1) b))))

(define (sum-squares a b)
    (if 
        (> a b) 
        0
        (+ (* a a) (sum-squares (+ a 1) b))))

(define (sum-it-iter a b)
    (define (run acc a)
        (if 
            (> a b) 
            acc
            (run (+ acc a) (+ a 1))))
    (run 0 a))

(define (sum-foo term next a b)
    (if
        (> a b)
        0
        (+ (term a) (sum-foo term next (next a) b))))

(sum-it 1 5) ; 15
(sum-it-iter 1 5) ; 15
(sum-squares 1 5) ; 15

; same with higher order functions
(define (identity a) a)
(define (square a) (* a a))
(define (inc a) (+ a 1))

(define (sum-it-foo a b) (sum-foo identity inc a b))
(define (sum-squares-foo a b) (sum-foo square inc a b))

(sum-it-foo 1 5)
(sum-squares-foo 1 5)

; lambdas
; (lambda (a b c) (+ a b c))
; (define (foo a b c) (+ a b c))
; equivalent to (define foo (lambda (a b c) (+ a b c)))

; testing let
(define (foobarz a) 
    (let 
        ([x 4]
        [y 1])
        (+ x y a))) ; this couldn't be a new let, as it's using previous vars

; vars can't be reused, as let is just syntactic sugar for lambda:
; (let 
;     ([a 4]
;     [b 2])
;     (+ a b))

; ((lambda (a b)
;     (+ a b)) 
;     4 2)

(foobarz 5)
