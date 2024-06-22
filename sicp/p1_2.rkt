#lang racket

(define (factorial x)
    (if (<= x 1)
        1
        (* x (factorial (- x 1)))))


; tail recurion (tail call optimization) can be applied - it is the last expression in recursion
; book calls this iterative process, while the other - recursive.
; (don't mix recursive process with recursive procedure. Both are recursive procedures actually)
;  We dont depend on stack frames to unwind and call intermediate
; results. This is self contained within the stack frame
(define (factorial-iter x)
    (define (it acc i)
        (if (> i x)
            acc
            (it (* acc i) (+ i 1))))
    (it 1 1))

(factorial 80)
(factorial-iter 80)


; this is tree recursion - it calls the same function multiple times and spreads. Some calculations can be performed
; multiple times here
(define (fibo n)
    (if (<= n 1) n
        (+ (fibo (- n 1))
            (fibo (- n 2)))))

