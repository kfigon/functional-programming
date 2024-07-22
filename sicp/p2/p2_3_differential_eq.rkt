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
          ((and (exponentiation? exp) (number? (exponent exp)))
                                    (make-product 
                                        (make-product (exponent exp) 
                                                      (make-exponentiation (base exp) 
                                                                           (- (exponent exp) 1)))
                                        (deriv (base exp) var)))
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


; ex 2.56
(newline)
(define (exponentiation? exp)
    (and (pair? exp) 
         (eq? (car exp) '**)))

(define (base exp) 
    (car (cdr exp)))

(define (exponent exp) 
    (car (cdr (cdr exp))))

(define (pow a x)
    (if (= x 0) 1
        (* a (pow a (- x 1)))))

(define (make-exponentiation a-base an-exponent)
    (cond 
        ((and (number? an-exponent) (= an-exponent 0)) 1)
        ((and (number? an-exponent) (= an-exponent 1)) a-base)
        ((and (number? a-base) (number? an-exponent)) (pow a-base an-exponent))
        (else (list '** a-base an-exponent))))

(deriv '(** x 1) 'x)
(deriv '(** x 2) 'x)
(deriv '(** x 3) 'x)