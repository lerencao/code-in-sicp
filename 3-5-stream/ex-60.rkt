#lang racket

(require "infinite-streams.rkt")
(provide mul-series)
(define (mul-series s1 s2)
  (stream-cons
   (* (stream-first s1) (stream-first s2))
   (add-streams
    (mul-series (stream-rest s1) (stream-rest s2))
    (add-streams
     (scale-stream (stream-rest s2) (stream-first s1))
     (scale-stream (stream-rest s1) (stream-first s2))))))
