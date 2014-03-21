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
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else (error "Unknown expression type" exp))))

; representing algebraic expressions
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (sum? e) (and (pair? e) (eq? (cadr e) '+)))
(define (make-sum x y)
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list x  '+ y))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (addend e) (car e))
(define (augend e) (caddr e))
(define (product? e) (and (pair? e) (eq? (cadr e) '*)))
(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list x '* y))))
(define (multiplier e) (car e))
(define (multiplicand e) (caddr e))

; exercise 2.56
(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list '** base exponent))))

; exercise 2.57
; see file changes...

; exercise 2.58

; a, see file changes...
