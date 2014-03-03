#lang racket

(define (full-fermat? n)
  (define (iter n a)
    (cond ((= a n) true)
          ((fermat-test n a) (iter n (+ a 1)))
          (else false)))
  (define (fermat-test n a) (= (expmod a n n) a))
  (define (expmod base exp n)
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) n))
            n))
          (else
           (remainder
            (* base (expmod base (- exp 1) n))
            n))))

  (iter n 2))