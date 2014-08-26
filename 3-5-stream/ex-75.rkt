#lang racket

(define (sign-change-detector current-value last-value)
  (cond [(and (< last-value 0) (> current-value 0)) 1]
        [(and (> last-value 0) (< current-value 0)) -1]
        [else 0]))

(define (make-zero-crossings
         input-stream last-value last-avpt)
  (let ((avpt
         (/ (+ (stream-first input-stream)
               last-value)
            2)))
    (stream-cons
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings
      (stream-rest input-stream)
      (stream-first input-stream)
      avpt))))
