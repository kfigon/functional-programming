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

(newline)
(define (map-tree tree fn)
    (cond ((null? tree) null)
          ((not (pair? tree)) (fn tree))
          (else (cons (map-tree (car tree) fn) (map-tree (cdr tree) fn)))))

(map-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) (lambda (x) (* x 10))) ;(10 (20 (30 40) 50) (60 70))
(map-tree a-tree (lambda (x) (* x 3)))