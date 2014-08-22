#lang racket


(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-first s1)
       (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map
     (lambda (x) (list (stream-first s) x))
     (stream-rest t))
    (interleave
     (stream-map
      (lambda (x) (list x (stream-first t)))
      (stream-rest s))
     (pairs (stream-rest s) (stream-rest t))))))
