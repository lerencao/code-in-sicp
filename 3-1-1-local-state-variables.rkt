#lang racket

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
   (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unkown request: MAKE-ACCOUNT" m ))))
  dispatch)


;;; accumulator
(define (make-accumulator init)
  (let ((acc init))
    (lambda (num)
      (set! acc (+ acc num))
      acc)))


;;; monitor
(define (make-monitored proc)
  (let ((called-times 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) called-times)
             ((eq? arg 'reset-count) (set! called-times 0))
             (else (begin (set! called-times (+ called-times 1))
                          (proc arg)))))))
