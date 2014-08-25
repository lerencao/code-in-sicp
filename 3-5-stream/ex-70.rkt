#lang racket

(define (merge-weighted s1 s2 weight)
  (cond
    [(stream-empty? s1) s2]
    [(stream-empty? s2) s1]
    [else
     (let* ([s1car (stream-first s1)]
            [s2car (stream-first s2)]
            [weighted-s1car (weight (stream-first s1))]
            [weighted-s2car (weight (stream-first s2))])
       (cond [(< weighted-s1car weighted-s2car)
              (stream-cons s1car
                           (merge-weighted (stream-rest s1) s2 weight))]
             [(> weighted-s1car weighted-s2car)
              (stream-cons s2car
                           (merge-weighted s1 (stream-rest s2) weight))]
             [else
              (stream-cons s1car
                           (merge-weighted
                            (stream-rest s1)
                            (stream-rest s2)
                            weight))]))]))

(define (weighted-pair s1 s2 weight)
  (stream-cons
   (list (stream-first s1) (stream-first s2))
   (merge-weighted
    (stream-map
     (lambda (x) (list (stream-first s1) x))
     (stream-rest s2))
    (weighted-pair
     (stream-rest s1)
     (stream-rest s2)
     weight)
    weight)))


;;; infinite integers from 1
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define int-pairs
  (weighted-pair
   integers
   integers
   (lambda (lst) (+ (car lst) (cadr lst)))))

(require "ex-56.rkt")
(define 235-pairs
  (weighted-pair
   (stream-rest S)
   (stream-rest S)
   (lambda (lst)
           (+ (* 2 (car lst))
              (* 3 (cadr lst))
              ( * 5 (car lst) (cadr lst))))))
