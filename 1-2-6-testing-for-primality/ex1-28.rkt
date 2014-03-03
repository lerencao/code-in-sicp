#lang racket

(define (full-fermat? n)
  (define (iter n a)
    (cond ((= a n) true)
          ((miller-rabin-test n a) (iter n (+ a 1)))
          (else false)))
  (define (miller-rabin-test n a) (= (expmod a (- n 1) n) 1))
  (define (expmod base exp n)
    (define (square-check x n)
      (if (and (not (or (= x 1) (= x (- n 1))))
               (= (remainder (* x x) n) 1))
          0
          (* x x)))
    (define (square x) (* x x))
    (cond ((= exp 0) 1)
          ((even? exp)
           
           (remainder
            (square-check (expmod base (/ exp 2) n) n)
            n))
          (else
           (remainder
            (* base (expmod base (- exp 1) n))
            n))))

  (iter n 2))