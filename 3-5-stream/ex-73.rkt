#lang racket

(require "utils/streams-ops.rkt")
(define (RC r c dt)
  (lambda (i v0)
    (define integral
      (stream-cons v0
                   (add-streams
                    (scale-stream * i (/ dt c))
                    integral)))
    (add-streams integral
                 (scale-stream * i r))))
