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

(newline)
; 2.17
; return list with only the last element
(define (last-pair aList)
    (let ([next (cdr aList)])
        (if (null? next) aList
            (last-pair next))))

(last-pair (list 1 2 3 4))
(last-pair (list 1))
(last-pair (list 1 2))

(newline)
; 2.18
(define (reverse tab)
    (define (run tab result)
        (if (null? tab) result
            (run (cdr tab) (cons (car tab) result)))) ; go all the way down, then cons up
    (run tab null))


(reverse (list 1 2 3 4))
(reverse (list 1 2 3))
(reverse (list 1 2))
(reverse (list 1))


; 2.19
; previous ex reiterated with any coin structure
(define (currency-exchange amount coins)
    (cond 
          ((= amount 0) 1)
          ((< amount 0) 0)
          ((null? coins) 0)
          (else (+ 
                    (currency-exchange (- amount (car coins)) coins)
                    (currency-exchange amount (cdr coins))))))


(currency-exchange 100 (list 50 25 10 5 1))
(currency-exchange 100 (list 100 50 20 10 5 2 1 0.5))

; 2.20
(define (sum-them init . rest)
    (define (run d acc)
        (if (null? d) acc
            (run (cdr d) (+ acc (car d)))))
    (run rest init))

(sum-them 3 1 2 3) ; 3 + 6

(printf "\nsame parity\n")
(define (same-parity first . rest)
    (define (run tab results)
        (cond 
            ((null? tab) results)
            ((= (modulo (car tab) 2) (modulo first 2)) (run (cdr tab) (cons (car tab) results)))
            (else (run (cdr tab) results))))
    (run rest (list first)))

(same-parity 1 2 3 4 5 6 7) ;(1 3 5 7)
(same-parity 2 3 4 5 6 7) ;(2 4 6)

; 2.21
(define (square-list items)
    (if (null? items) null
        (cons (* (car items) (car items))
              (square-list (cdr items)))))

(define (map-list items fn)
    (if (null? items) null
        (cons (fn (car items))
              (map-list (cdr items) fn))))

(square-list (list 1 2 3 4))
(map-list (list 1 2 3 4) (lambda (x) (* x x)))

; 2.22
; this is applying procedure backwards
(define (square-list-iter items)
    (define (run items result)
        (if (null? items) result
            (run (cdr items)
                 (cons (* (car items) (car items))
                       result))))
    (run items null))
    ; changing the order does not help because then we append list to a single items - not correct, should be item to a list

(square-list-iter (list 1 2 3 4))

; 2.23
(define (for-eachx items fn)
    (cond  ; if for some reason is not working , I can't figure out why
        ((null? items) #t)
        (else 
            (fn (car items))
            (for-eachx (cdr items) fn))))

(for-eachx (list 1 2 3 4 5) 
          (lambda (x) 
                (newline)
                (display x)))
    
; 2.25
; extract 7s
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

; 2.27
(newline)
(define (deep-reverse x)
    (define (run x result)
        (cond 
              ((null? x) result)
              ((not (pair? (car x))) (run (cdr x) (cons (car x) result))) ; regular reverse
              (else (run (cdr x) (cons (deep-reverse (car x)) result))))) ; reduce down - same as reverse, but deep-reverse head
    (run x null))

(deep-reverse (list 1 2 3))
(deep-reverse (list (list 1 2) 3))
(deep-reverse (list (list 1 2) (list 3 4)))  ;((4 3) (2 1))
(deep-reverse (list (list 1 2) (list (list 3 4) 5)))