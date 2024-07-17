#lang racket

(define (map iter fn)
    (if (null? iter) null
        (cons (fn (car iter)) (map (cdr iter) fn))))

(define (filter iter fn)
    (if (null? iter) null
        (if (fn (car iter)) 
            (cons (car iter) (filter (cdr iter) fn))
            (filter (cdr iter) fn))))

(map (list 1 2 3 4 5 6) (lambda (x) (* x 3)))

(define (even v) (= (modulo v 2) 0))
(filter (list 1 2 3 4 5 6) even)