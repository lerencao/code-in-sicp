#lang racket
;;; linear-resursive
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

;;; iterative
(define (product-iter term a next b)
  (define (iter a mem)
    (if (> a b)
        mem
        (iter (next a) (* mem (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter  ;change to product-recur to use recursive version
   (lambda (x) x)
   1
   (lambda (k) (+ k 1))
   n))

(define (pi n)
  (define (term x)
    (if (= (remainder x 2) 1)
        (/ (+ x 1) (+ x 2))
        (/ (+ x 2) (+ x 1))))
  (* 4.0
     (product-iter ; similar
      term
      1
      (lambda (k) (+ k 1))
      n)))
