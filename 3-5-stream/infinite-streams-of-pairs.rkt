#lang racket

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map
     (lambda (x) (list (stream-first s) x))
     (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons
       (stream-first s1)
       (interleave s2 (stream-rest s1)))))

(provide interleave pairs)



;;; testing code
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define int-pairs
  (pairs integers integers))
(define (before ls)
  (define (counting n p)
    (display (stream-first p))
    (display "\n")
    (if (equal? (stream-first p) ls)
        n
        (counting (+ n 1) (stream-rest p))))
  (counting 0 int-pairs))
(before '(1 100))
