#lang planet neil/sicp

; the differentiation programs with abstract data
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var)))
        ((product? exp) (make-sum
                          (make-product (deriv (multiplier exp) var)
                                        (multiplicand exp))
                          (make-product (deriv (multiplicand exp) var)
                                        (multiplier exp))))
        (else (error "Unknown expression type" exp))))

; representing algebraic expressions
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (sum? e) (and (pair? e) (eq? (car e) '+)))
(define (make-sum x y) (list '+ x y))
(define (addend e) (cadr e))
(define (augend e) (caddr e))
(define (product? e) (and (pair? e) (eq? (car e) '*)))
(define (make-product x y) (list '* x y))
(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))
