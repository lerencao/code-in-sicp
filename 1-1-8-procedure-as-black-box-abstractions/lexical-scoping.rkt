#lang racket


(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (average x y)
    (/ (+ x y) 2))
  
  (sqrt-iter 1.0))