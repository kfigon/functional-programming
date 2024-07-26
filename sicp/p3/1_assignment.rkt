#lang racket

(define balance 100)

(define (withdraw amount)
    (if (>= balance amount) 
        (begin ; begin returns the last expression in sequence
            (set! balance (- balance amount))
            balance) ; set! to modify the variable - special form
        "Insufficient funds"))
        

(withdraw 50)
(withdraw 50)
(withdraw 1)

; better way without global variables - capture the state inside the funciton

(define new-withdraw
  (let ([balance 100])
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(newline)
(new-withdraw 50)
(new-withdraw 50)
(new-withdraw 1)

(newline)
(define (make-withdraw balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds")))

(define account (make-withdraw 100))
(account 50)
(account 50)
(account 1)


(newline)
(define (make-account balance)
    (define (debit amount)
        (if (>= balance amount)
            (begin 
                (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    
    (define (credit amount)
        (begin 
            (set! balance (+ balance amount))
            balance))
    
    (define (dispatch op)
        (cond 
            ((eq? op 'credit) credit)
            ((eq? op 'debit) debit)
            (else (error "invalid operation"))))
    dispatch)

(define acc (make-account 100))
((acc 'debit) 99)
((acc 'debit) 5)
((acc 'credit) 39)
((acc 'debit) 40)