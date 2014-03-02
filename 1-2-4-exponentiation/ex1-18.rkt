#lang racket
(define (fast-mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (mult-iter s a b)
    (cond ((= b 1) (+ s a))
          ((even? b) (mult-iter s (double a) (halve b)))
          (else (mult-iter (+ s a) (double a) (halve (- b 1))))))
  (mult-iter 0 a b))