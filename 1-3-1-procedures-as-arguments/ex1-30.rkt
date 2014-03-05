#lang racket

(define (sum term a next b)
  (define (iter k result)
    (if (> k b)
        result
        (iter (next k) (+ result (term k)))))
  (iter a 0))

;;; a test the iterative sum
(define (cube-sum a b)
  (sum (lambda (x) (* x x x))
       a
       (lambda (k) (+ k 1))
       b))
