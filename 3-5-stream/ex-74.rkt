#lang racket

(define (sign-change-detector current-value last-value)
  (cond [(and (< last-value 0) (> current-value 0)) 1]
        [(and (> last-value 0) (< current-value 0)) -1]
        [else 0]))

(define zero-crossings
  (stream-map-extended sign-change-detector
                       sense-data
                       (stream-cons 0 sense-data)))
