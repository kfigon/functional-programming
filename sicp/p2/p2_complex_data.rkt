#lang racket

; library for ration numbers

(define (make-rational a b) (cons a b)) ; this could be enhanced to use lower terms. Sign could also be normalized
(define (num rat) (car rat))
(define (denom rat) (cdr rat))

; more performant and clever alternatives
; (define make-rational cons)
; (define num car)
; (define denom cdr)

; ax/ay + bx/by = ((axby) + (bxay))/(ay*by)
(define (add-rat a b) 
    (make-rational 
        (+ (* (num a) (denom b))
           (* (num b) (denom a)))
        (* (denom a) (denom b))))

(define (sub-rat a b) 
    (make-rational 
        (- (* (num a) (denom b))
           (* (num b) (denom a)))
        (* (denom a) (denom b))))

(define (mult-rat a b) 
    (make-rational 
                    (* (num a) (num b))
                    (* (denom a) (denom b))))

(define (eq-rat a b) 
    (= (* (num a) (denom b)) 
       (* (num b) (denom a))))


(define (print-rat x)
    (newline)
    (display (num x))
    (display "/")
    (display (denom x))
    (newline))

(print-rat (add-rat (make-rational 1 2) (make-rational 3 4)))
(print-rat (add-rat (make-rational 1 3) (make-rational 1 3)))


; compound structures using pairs - list
; (cons 1 (cons 2 (cons 3 null))) == (list 1 2 3)
; (car (cdr (cdr x))) will get us 3
; (cadr x) == (car (cdr x))
; (pair? v) - to check if it's a pair

(define (get-nth tab i)
    (if (= i 0) 
        (car tab)
        (get-nth (cdr tab) (- i 1))))

(printf "\nget-nth\n")
; (null? aList) to check if it's empty
(get-nth (list 1 2 3) 0) ; -> 1
(get-nth (list 1 2 3) 1) ; -> 2
(get-nth (list 1 2 3) 2) ; -> 3
; (get-nth (list 1 2 3) 3) ; -> err

; can be also iterative of course
(define (list-len tab) 
    (if (null? tab) 
        0
        (+ 1 (list-len (cdr tab)))))

(newline)
(list-len null)
(list-len (list 1))
(list-len (list 1 2 3))


(define (append l1 l2)
    (if (null? l1) l2
        (cons (car l1) (append (cdr l1) l2))))

(append (list 1 2 3) (list 4 5 6))
(append (list 1) (list 2 3 4 5 6))
(append null (list 1 2 3 4 5 6))
(append (list 1 2 3 4 5) (list 6))
(append (list 1 2 3 4 5 6) null)

(define (scale-list items factor)
    (if (null? items) null
        (cons (* 
                factor (car items))
                (scale-list (cdr items) factor))))

(printf "\nscale-list\n")
(scale-list (list 1 2 3 4 5) 5)

(printf "\nmap\n")

(define (map-list items fn)
    (if (null? items) null
        (cons (fn (car items)) 
              (map-list (cdr items) fn))))

(map-list (list 1 2 3 4 5) (lambda (x) (* x 5)))


(printf "\nleft/right maps\n")
(define (process-left fn vs)
  (define (iter vs result)
    (if (null? vs) result
        (cons (fn (car vs)) (iter (cdr vs) result))))
  (iter vs null))

(define (process-right fn vs)
  (define (iter vs result)
    (if (null? vs) result
        (iter (cdr vs) (cons (fn (car vs)) result))))
  (iter vs null))

(process-left (lambda (x) (* x 2)) (list 1 2 3 4 5))
(process-right (lambda (x) (* x 2)) (list 1 2 3 4 5))