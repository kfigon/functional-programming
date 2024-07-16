#lang racket

; trees

(define a-tree (cons (list 1 2) (list 3 4)))
(display a-tree) ; ((1 2) 3 4)
(newline)

(define (tree-len t)
    (cond 
          ((null? t) 0)
          ((not (pair? t)) 1)
          (else (+ (tree-len (car t)) (tree-len (cdr t))))))
(tree-len a-tree)
