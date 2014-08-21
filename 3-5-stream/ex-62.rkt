#lang racket

(require "ex-59.rkt")
(require "ex-60.rkt")
(require "ex-61.rkt")
(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
      (error "the denominator has a zero constant term")
      (mul-series
       s1
       (invert-unit-series s2))
      ))

(define tan-series
  (div-series sine-series cosine-series))
