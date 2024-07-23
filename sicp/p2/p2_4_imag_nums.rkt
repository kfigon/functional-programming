#lang racket

(define (square x) (expt x 2))

;;;;;;;;;;;;;;;;;;;  generic type tags - tag the data and dispatch accordingly

(define (tag-data tag data) (cons tag data))

(define (get-tag data) 
    (if (pair? data) (car data)
        (error "not a pair")))

(define (get-data data) 
    (if (pair? data) (cdr data)
        (error "not a pair")))

(define (rectangular? data) (eq? (get-tag data) 'rect))
(define (polar? data) (eq? (get-tag data) 'polar))
;;;;;;;;;;;;;;;;;;;;;;;

; we'll have 2 internal representations available
(define (make-from-rectangular real imag)
    (tag-data 'rect (cons real imag)))

(define (make-from-polar mag angle)
    (tag-data 'polar (cons mag angle)))

(define (real z)
    (cond 
        ((rectangular? z) (real-form-rectangular z))
        ((polar? z) z) ; todo
        (else (error "invalid tag"))))

(define (imag z) z)

(define (magnitude z) z)
(define (angle z) z)

(define (real-form-rectangular z) (car z))
(define (imag-form-rectangular z) (cdr z))

(define (magnitude-from-rectangular z)
  (sqrt (+ (square (real-form-rectangular z))
           (square (imag-form-rectangular z)))))

(define (angle-from-rectangular z)
  (atan (imag-from-rectangular z)
        (real-from-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rect (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rect
   (cons (* r (cos a)) (* r (sin a)))))


(define (add z1 z2)
    (make-from-rectangular (+ (real z1) (real z2))
                           (+ (imag z1) (imag z2))))
(define (sub z1 z2)
    (make-from-rectangular (- (real z1) (real z2))
                           (- (imag z1) (imag z2))))

(define (mul z1 z2)
  (make-from-polar (* (magnitude z1) (magnitude z2))
                   (+ (angle z1) (angle z2))))

(define (div z1 z2)
  (make-from-polar (/ (magnitude z1) (magnitude z2))
                   (- (angle z1) (angle z2))))


; data directed programming - oop with internal procedures and tags. We can define next for polar representation

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error
                "No method for these types: 
                APPLY-GENERIC"
                (list op type-tags))))))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))

; etc