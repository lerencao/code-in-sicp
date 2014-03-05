#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate
                          combiner
                          null-value
                          term
                          (next a)
                          next
                          b))))

;;; iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a mem)
    (if (> a b)
        mem
        (iter (next a) (combiner mem (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter (lambda (a b) (+ a b)) 0
              term a next b))

(define (product term a next b)
  (accumulate-iter (lambda (a b) (* a b)) 1
              term a next b))


;;; procedure used for test

(define (sum-integer a b)
  (sum (lambda (x) x) a (lambda (x) (+ x 1)) b))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))
