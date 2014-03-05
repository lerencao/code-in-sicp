#lang planet neil/sicp

(define (filtered-accumulate combiner null-value term a next b pred)
  (if (> a b)
      null-value
      (if (pred a)
          (combiner (term a) (filtered-accumulate
                              combiner
                              null-value
                              term
                              (next a)
                              next
                              b
                              pred))
          (filtered-accumulate combiner null-value term (next a) next b pred))
      ))

;;; iterative process
(define (filtered-accumulate-iter combiner null-value term a next b pred)
  (define (iter a mem)
    (if (> a b)
        mem
        (iter (next a)
              (if (pred a) (combiner mem (term a)) mem))))
  (iter a null-value))

;(define (prime-square-sum a b)
;  (filtered-accumulate-iter
;   (lambda (a b) (+ a b))
;   0
;   (lambda (x) (* x x))
;   a
;   (lambda (k) (+ k 1))
;   b
;   (lambda (x) (prime? x)))) ; the definition of prime? is missing

(define (relative-prime-product n)
  (filtered-accumulate ; change to iterative version
   (lambda (a b) (* a b))
   1
   (lambda (x) x)
   2
   (lambda (k) (+ k 1))
   n
   (lambda (x) (= (gcd x n) 1))))
