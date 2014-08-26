#lang racket

(require "utils/streams-ops.rkt")
(define (sign-change-detector current-value last-value)
  (cond [(and (< last-value 0) (> current-value 0)) 1]
        [(and (> last-value 0) (< current-value 0)) -1]
        [else 0]))


(define (smooth s)
  (stream-map-extended
   (lambda (x1 x2) (/ (+ x1 x2) 2))
   (stream-cons 0 s)
   s))

(define (make-zero-crossings
         input-stream smoothing)
    (stream-map-extended
     sign-change-detector
     (stream-cons 0 (smoothing input-stream))
     (smoothing input-stream)))
;;; (make-zero-crossings sense-data smooth)
