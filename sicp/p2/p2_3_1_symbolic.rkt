#lang racket

(define a 1)
(define b 2)

(list a b) ; (1 2)  - evaluated
(list 'a 'b) ; (a b) - this is not evaluated, these are just symbols
(list 'a b) ; (a 2)

; c is not defined, doesn't matter - we don't care about the value - just the symbol
(car '(a b c)) ;a
(cdr '(a b c)) ;(b c)

; '() == null as empty list

; eq? - compares 2 symbols, tests if the values are the same

; return false or sublist where the element is contained
(define (memq item xs)
    (cond ((null? xs) #f)
          ((eq? item (car xs)) xs)
          (else (memq item (cdr xs)))))

(memq 'apple '(x (apple sauce) y apple pear)) ; (apple pear) - the last one
(memq 'apple '(x apple y foo bar)) ; (apple y foo bar)

(define (map fn iter)
    (if (null? iter) null
        (cons (fn (car iter)) (map fn (cdr iter)))))

; same thing. Adding any symbol will break though
(map (lambda (x) (* x 2)) '(1 2 3 4 5))
(map (lambda (x) (* x 2)) (list 1 2 3 4 5))

