#lang racket

(require "exploiting-the-stream-paradigm.rkt")

(define (stream-limit s tolerance)
  (let ([a (stream-ref s 0)]
        [b (stream-ref s 1)])
    (if (< (abs (- a b)) tolerance)
        b
        (stream-limit
         (stream-rest s)
         tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
