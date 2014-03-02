#lang racket
(define (fast-mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult (double a) (halve (- b 1)))))))