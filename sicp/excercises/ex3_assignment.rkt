#lang racket

; 3.2

(define (monitored-proc proc)
    (let ([invocations 0])
        (define (dispatch arg)
            (cond ((eq? arg 'how-many) invocations)
                  ((eq? arg 'reset) (begin (set! invocations 0) invocations))
                  (else 
                    (begin (set! invocations (+ invocations 1))
                           (proc arg)))))
        
        dispatch))

(define (inc a) (+ a 1))

(define monitored-inc (monitored-proc inc))

(monitored-inc 12) ;13
(monitored-inc 14) ;15
(monitored-inc 'how-many) ; 2
(monitored-inc 15) ; 16
(monitored-inc 'reset) ; 0
(monitored-inc 16) ; 17
(monitored-inc 'how-many) ;1