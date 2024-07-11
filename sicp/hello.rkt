#lang racket

(printf "hello, some expressions:\n")
(+ 1 3 5 4 3 1)
(+  (* 3
        (+  (* 2 4) 
            (+ 3 5)))
    (+  (- 10 7) 
        6))

(printf "variable:\n")
; variable. Not evaluated, this is special form
(define myvar 5)
(+ myvar 2)

(printf "functions:\n")
; function
(define (square x) (* x x))
(square 4)

(define (sum-of-squares x y) 
    (+ (square x) (square y)))

(sum-of-squares 12 4)

(printf "abs\n")
(define (abs x)
    (cond ((> x 0) x) ; Switch statement
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
    (cond ((>= x 0) x)
          (else (- x)))) ; with default

(define (abs3 x) ; if-else
  (if (< x 0) 
      (- x)
      x))
; (and a b c d e)
; (or a b c d e)
; (not a)

(abs2 4)
(abs2 (- 1))
(abs2 0)

(define (fizz-buzz x) 
    (cond 
        ((= (modulo x 15) 0) "fizzbuzz")
        ((= (modulo x 5) 0) "fizz")
        ((= (modulo x 3) 0) "buzz")
        (else (format "~v" x))))

(define (fizz-buzz2 x) 
    (if (= (modulo x 15) 0) "fizbuzz"
        (if (= (modulo x 5) 0) "fizz"
        (if (= (modulo x 3) 0) "buzz"
        (format "~v" x)))))

(fizz-buzz2 12)
(fizz-buzz2 10)
(fizz-buzz2 5)
(fizz-buzz2 15)
(fizz-buzz2 2)

; (lambda (a b) (+ a b))
; let - to define many local variables instead of define
; (let ([a 1]
;       [b 3])
;       (+ a b 4)))

(define a-pair (cons 1 2))
(car a-pair) ; first element of pair
(cdr a-pair) ; second

(newline)
; variadic arguments - dot notation
(define (foo a b . other) a)
(foo 1 2 3 4 5 6) ; a =1, b = 2, c = (list 4 5 6)