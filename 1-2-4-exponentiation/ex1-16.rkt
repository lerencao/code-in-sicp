#lang racket
(define (fast-expt b n)
  (define (expt-iter a b n)
    (cond ((= n 1) (* a b))
          ((even? n) (expt-iter a (* b b) (/ n 2)))
          (else (expt-iter (* a b) (* b b) (/ (- n 1) 2)))))
  (expt-iter 1 b n))