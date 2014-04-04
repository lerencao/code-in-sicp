#lang racket

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))


;;; shared account
(define (make-joint acc password new-password)
  (lambda (pass m)
    (if (eq? pass new-password)
        (acc password m)
        (error "Wrong password" pass))))

;;; f
(define (f n)
  (let ((state 1))
    (begin (set! state (* state n))
           state)))
