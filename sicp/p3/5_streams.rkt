#lang racket

; infinite streams of data, delayed evaluation

; special form that creates lazy field - just a function
; it's also good to memoize the data, so it's safe and efficient to evaluate multiple times
(define (delay data) (lambda () data))

; to evaluate lazy data
(define (force data) (data))

(define (cons-stream head tail)
    (cons head (delay tail)))

(define (car-stream s) (car s))
(define (cdr-stream s) (force (cdr s)))

