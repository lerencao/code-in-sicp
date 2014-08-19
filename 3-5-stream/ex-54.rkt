#lang racket

(require "ex-50.rkt")
(require "infinite-streams.rkt")

(provide mul-streams factorials)
(define (mul-streams s1 s2)
  (stream-map-extended * s1 s2))
(define factorials
  (stream-cons 1
               (mul-streams factorials
                            (stream-rest integers))))
