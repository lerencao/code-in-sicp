#lang racket

(require "infinite-streams.rkt")
(define (partial-sums s)
  (stream-cons
   (stream-first s)
   (add-streams (partial-sums s)
               (stream-rest s))))

(provide partial-sums)