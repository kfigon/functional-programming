#lang racket

(define a-tree (list 1 2 3 (list 4 5) (list 6 7 (list 8 9) 10)))

(define (flatten tree)
    (define (run tree result)
        (cond 
              ((null? tree) result)
              ((not (pair? tree)) (cons tree result))
              (else (run (car tree) (run (cdr tree) result)))))
    (run tree null))

(flatten a-tree)

(newline)
(define (deep-reverse tree)
    (define (iter tree result)
        (cond ((null? tree) result)
              ((not (pair? (car tree))) (iter (cdr tree) (cons (car tree) result)))
              (else (iter (cdr tree) (cons (deep-reverse (car tree)) result)))))
    (iter tree null))

a-tree
(deep-reverse a-tree)

(define (count-leaves tree) 
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ 
                    (count-leaves (car tree))
                    (count-leaves (cdr tree))))))

(count-leaves a-tree)