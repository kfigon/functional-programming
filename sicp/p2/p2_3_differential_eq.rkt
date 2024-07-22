#lang racket

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp) (make-sum (make-product (deriv (multiplicand exp) var)
                                                  (multiplier exp))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp))))
           (else (error "unknown exp: " exp))))

(define variable? symbol?)
(define (same-variable? v1 v2)
    (and (variable? v1) 
         (variable? v2) 
         (eq? v1 v2)))

(define (sum? exp)
    (and (pair? exp) 
         (eq? (car exp) '+)))

(define (product? exp)
    (and (pair? exp) 
         (eq? (car exp) '*)))

(define (make-sum e1 e2) 
    (cond 
        ((and (number? e1) (= e1 0)) e2)
        ((and (number? e2) (= e2 0)) e1)
        ((and (number? e2) (number? e1)) (+ e1 e2))
        (else (list '+ e1 e2))))

(define (make-product e1 e2) 
    (cond 
        ((or (and (number? e1) (= e1 0)) 
             (and (number? e2) (= e2 0))) 0)
        ((and (number? e1) (= e1 1)) e2)
        ((and (number? e2) (= e2 1)) e1)
        ((and (number? e2) (number? e1)) (* e1 e2))
        (else (list '* e1 e2))))

(define (addend s) (car (cdr s))) ;first part of sum
(define (augend s) (car (cdr (cdr s)))) ;second part of sum
(define (multiplicand s) (car (cdr s)))
(define (multiplier s) (car (cdr (cdr s))))


(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; 'y
(deriv '(* (* x y) (+ x 3)) 'x) ;(+ (* x y) (* y (+ x 3)))

