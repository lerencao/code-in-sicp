#lang racket

(require "ex-55.rkt")
(require "exploiting-the-stream-paradigm.rkt")
(define (log2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map -
                           (log2-summands (+ n 1)))))
(define log2
  (partial-sums (log2-summands 1)))

(define log2-euler
  (euler-transform log2))

(define log2-accelerated
  (accelerated-sequence euler-transform log2))
