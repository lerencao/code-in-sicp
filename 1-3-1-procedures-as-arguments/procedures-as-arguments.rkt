#lang racket

; cube definition
(define (cube x) (* x x x))

; sum from a to b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; sum cube of a to b
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

; a formula to converge to pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; common pattern of abstraction
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))
(define (sum-cubes a b) (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b) (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(display (integral cube 0 1 0.01))
(display (integral cube 0 1 0.001))
