#lang racket

(define (map fn iter)
    (if (null? iter) null
        (cons (fn (car iter)) (map fn (cdr iter)))))

(define (filter fn iter)
    (if (null? iter) null
        (if (fn (car iter)) 
            (cons (car iter) (filter fn (cdr iter)))
            (filter fn (cdr iter)))))

(define (reduce init fn iter)
    (if (null? iter) 
        init
        (fn (car iter) 
            (reduce init fn (cdr iter)))))

(define (enumerate-num low high)
    (if (> low high) null
        (cons low (enumerate-num (+ low 1) high))))

(define (enumerate-tree tree) ; fringe from exercises
    (define (iter tree result)
        (cond ((null? tree) result)
              ((not (pair? tree)) (cons tree result))
              (else (iter (car tree) (iter (cdr tree) result)))))

    (iter tree null))

(define a-list (list 1 2 3 (list 4 5) (list (list 6) 7)))
(display a-list)
(newline)
(enumerate-tree a-list)
(map (lambda (x) (* x 3)) (list 1 2 3 4 5 6))

(define (even? v) (= (modulo v 2) 0))
(define (odd? v) (not (even? v)))

(filter even? (list 1 2 3 4 5 6))
(reduce 0 + (list 1 2 3 4 5))
(reduce 1 * (list 1 2 3 4 5))
(reduce null cons (list 1 2 3 4 5))

(newline)
(define (square v) (* v v))
(define (sum-odd-squares top)
    (reduce 0 + 
        (map square 
            (filter odd? 
                (enumerate-num 1 top)))))

(sum-odd-squares 6)