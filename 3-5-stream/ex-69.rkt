#lang racket

(require "infinite-streams-of-pairs.rkt")

(define (triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map
     (lambda (x) (cons (stream-first s) x))
     (stream-rest (pairs t u)))
    (triples
     (stream-rest s)
     (stream-rest t)
     (stream-rest u)))))

;;;
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define int-triples (triples integers integers integers))
(define pythagoreans
  (stream-filter
   (lambda (triple)
     (let ([a (car triple)]
           [b (cadr triple)]
           [c (caddr triple)])
       (= (+ (* a a) (* b b)) (* c c))))
   int-triples))
