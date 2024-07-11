#lang racket

; 2.2
; segments on a plane
(define make-point cons)
(define get-x car)
(define get-y cdr)

(define make-segment cons)
(define get-start car)
(define get-end cdr)

(define (average a b) (/ (+ a b) 2))
(define (average-point a b) 
    (make-point (average (get-x a) (get-x b))
                (average (get-y a) (get-y b))))

(define (midpoint seg)
    (average-point (get-start seg) (get-end seg)))

(define (print-point a)
    (display "(")
    (display (get-x a))
    (display ",")
    (display (get-y a))
    (display ")")
    (newline))

(let ([a (make-point 1.0 2.0)]
      [b (make-point 18.0 4.0)])
      (print-point (midpoint (make-segment a b))))


; 2.4
; alternative cons car
(define (cons2 x y)
    (lambda (m) (m x y))) ; return a function which takes another function (m) that is applied to x y
    ; type pair func(func(int,int)int)int
	; cons := func(a int, b int) pair {
	; 	return func(selector func(int, int) int) int { return selector(a,b) }
    ; }

	; car := func(z pair) int {
	; 	return z(func (a int, b int) int { return a })
	; }

	; cdr := func(z pair) int {
	; 	return z(func (a int, b int) int { return b })
	; }
(define (car2 z)
    (z (lambda (p q) p))) ; z is a function that takes 2 args and can return something

(define (cdr2 z)
    (z (lambda (p q) q)))

(car2 (cons2 1 2))
(cdr2 (cons2 1 2))


; 2.5
; represent pairs of non neg integers as product 2^a*3^b

(define (pow a b)
    (define (run acc ith)
        (if (= ith b) acc 
            (run (* acc a) (+ ith 1))))
    (run 1 0))

(define (int-cons a b) 
    (* (pow 2 a) (pow 3 b)))

; todo: math... get a and b from the magic num
; (define (int-car x) x)