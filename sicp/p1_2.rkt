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

(define (fibo-iter n)
    (define (it a b i) 
        (if (>= i n) b
            (it b (+ a b) (+ i 1))))
    (if (< n 2)
        n
        (it 0 1 1)))

(printf "fibo iter:\n")
(fibo-iter 8) ;21
(fibo-iter 9) ;34

(printf "coin exchange:\n")

(define (coin-exchange amount)
    (define (cc amount coins)
        (cond 
            ((= amount 0) 1)
            ((< amount 0) 0)
            ((<= coins 0) 0)
            (else (+ 
                    (cc amount (- coins 1))                  ; we didn't use given coin
                    (cc (- amount (get-coin coins)) coins))))) ; we used given coin
    (cc amount 5))

; we don't know arrays yet, function to simulate it
(define (get-coin idx)
    (cond 
        ((= idx 1) 1)
        ((= idx 2) 2)
        ((= idx 3) 5)
        ((= idx 4) 10)
        ((= idx 5) 20)))


(coin-exchange 61)