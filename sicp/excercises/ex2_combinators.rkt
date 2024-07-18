#lang racket

(define (reduce init fn iter)
    (if (null? iter) init
        (fn (car iter) 
            (reduce init fn (cdr iter)))))

; 2.33
(define (map p sequence)
  (reduce null (lambda (x y) (cons (p x) y)) sequence))
(define (append seq1 seq2)
  (reduce seq2 cons seq1))
(define (length sequence)
  (reduce 0 (lambda (x acc) (+ acc 1)) sequence))

(map (lambda (x) (* x 3)) (list 1 2 3 4 5 6))
(append (list 1 2 3) (list 4 5 6))
(append (list 1 2 3) null)
(append (list 1) (list 4 5 6))

(length (list 1 2 3 4 5 6))

; 2.35
; tree len as reduce
(newline)

(define (tree-len t)
    (cond 
          ((null? t) 0)
          ((not (pair? t)) 1)
          (else (+ (tree-len (car t)) (tree-len (cdr t))))))

(define (flat-tree tree)
    (define (iter tree result)
        (cond ((null? tree) result)
              ((not (pair? tree)) (cons tree result))
              (else (iter (car tree) (iter (cdr tree) result)))))
    (iter tree null))

(define (count-leaves t)
  (reduce 
        0
        (lambda (x acc) (+ acc 1))
        ; (map (lambda (x) x) (flat-tree t)))) ; flattening can be also done in more clever way
        (flat-tree t)))

(define a-tree (cons (list 1 2) (list 3 (list 4 5))))
(tree-len a-tree)
(count-leaves a-tree)