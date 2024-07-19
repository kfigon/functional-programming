#lang racket

; 2.53
(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ; '((george))
(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; '#f
(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)

; 2.54
(newline)
(define (equal? l1 l2)
    (cond ((null? l1) (null? l2))
          ((null? l2) (null? l1))
          ((not (pair? l1)) (and (not (pair? l2)) (eq? l1 l2)))
          (else (and 
                     (equal? (car l1) (car l2)) 
                     (equal? (cdr l1) (cdr l2))))))

(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this (is a) list) '(this (is a) list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f

; 2.55
(car ''abracadabra) ; 'quote
; because ' can equal a list
; so car first is stripping the first ', then we got a 'abracadabra => quote

; this is rewritten as
; (car '(quote abracadabra))
