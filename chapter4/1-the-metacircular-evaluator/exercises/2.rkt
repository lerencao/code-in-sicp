#lang racket

;;; a)
; statement types controlled by keywords
; have higher priority than normal procedure applications

;;; b)

; application: (call <operator> <operands>)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
