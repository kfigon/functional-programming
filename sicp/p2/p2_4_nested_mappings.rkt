#lang racket

(define (append l1 l2)
    (if (null? l1) l2
        (cons (car l1) (append (cdr l1) l2))))

(define (map fn iter)
    (if (null? iter) null
        (cons (fn (car iter)) (map fn (cdr iter)))))

(define (accumulate op initial sequence) 
  (if (null? sequence) initial 
   (op (car sequence) 
     (accumulate op initial (cdr sequence))))) 

(define (enumerate-range low high)
    (if (> low high) null
        (cons low (enumerate-range (+ low 1) high))))    

(define (flatmap fn iter) ; append and map is very frequent
    (accumulate append null (map fn iter)))

(define (nested n)
  (flatmap
    (lambda (x)
       (map (lambda (y) (list y x)) (enumerate-range 0 x)))
    (enumerate-range 0 n)))

(nested 5)