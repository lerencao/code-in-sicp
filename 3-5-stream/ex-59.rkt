#lang racket

(require "ex-50.rkt")
(require "infinite-streams.rkt")
(define (div-streams s1 s2)
  (stream-map-extended / s1 s2))

(define (integrate-series a)
  (if (stream-empty? a)
      a
      (div-streams a
                   integers)))

(define cosine-series
  (stream-cons 1
               (stream-map
                (lambda (x) (* -1 x))
                (integrate-series sine-series))))

(define sine-series
  (stream-cons 0
               ((integrate-series cosine-series))))
