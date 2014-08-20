#lang racket

(provide integers no-sevens fibs primes)
(provide add-streams scale-stream)
;;; infinite integers from 1
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;;;
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7)))
                 integers))


;;;
(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;;;
(define (sieve stream)
  (stream-cons
   (stream-first stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-first stream))))
           (stream-rest stream)))))
(define primes
  (sieve (integers-starting-from 2)))


;;; defining streams implicitly

(define ones (stream-cons 1 ones))

(require "ex-50.rkt")
(define (add-streams s1 s2)
  (stream-map-extended + s1 s2))
(define integers-another
  (stream-cons 1 (add-streams ones integers-another)))


(define fibs-additional
  (stream-cons
   0
   (stream-cons
    1
    (add-streams fibs-additional
                 (stream-rest fibs-additional)))))


(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))
(define double
  (stream-cons 1 (scale-stream double 2)))

;;; (require math/number-theory)
(define primes-additional
  (stream-cons 2
               (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (square x) (* x x))
  (define (iter ps)
    (cond
      [(> (square (stream-first ps)) n) true]
      [(divisible? n (stream-first ps)) false]
      [else (iter (stream-rest ps))]))
  (iter primes))
