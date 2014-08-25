#lang racket

(require "ex-70.rkt")

(define weight
  (lambda (lst)
     (+ (* (car lst) (car lst) (car lst))
        (* (cadr lst) (cadr lst) (cadr lst)))))

(define (ramanujan s)
    (let* ([a (stream-ref s 0)]
           [b (stream-ref s 1)]
           [c (stream-ref s 2)])
      (if (and (= (weight a) (weight b))
               (not (= (weight b) (weight c))))
          (stream-cons
           (weight a)
           (ramanujan (stream-rest (stream-rest s))))
          (ramanujan (stream-rest s)))))

(define ramanujan-numbers
  (ramanujan
   (weighted-pair integers integers weight)))
