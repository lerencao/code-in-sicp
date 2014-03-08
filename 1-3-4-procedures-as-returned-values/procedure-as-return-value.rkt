#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? x y) (< (abs (- x y)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next-guess (f guess)))
      (if (close-enough? next-guess guess)
          next-guess
          (try next-guess))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (sqrt x)
  ; see prev chap for fixed-point definition
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  ; see prev chap for fixed-point definition
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))

;;; Newton's method
(define (derivative g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
; ((derivative (lambda (x) (* x x x))) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((derivative g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newton-method (lambda (y) (- (* y y) x)) 1.0))

;;; abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (abstracted-sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp 1.0))
(define (abstracted-newton-sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (* y y) x))
   newton-transform 1.0))

;;; exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
; (newton-method (cubic 1 1 1) 1) => -0.9999999999997796

;;; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
; (((double (double double)) inc) 5) =>  21

;;; exercise 1.42
(define (compose f g)
  (lambda (x) ( f (g x))))
;((compose
;  (lambda (x) (* x x))
;  (lambda (x) (+ x 1))) 6) => 49

;;; exercise 1.43
(define (repeated f n)
  (if (= n 1) f
      (repeated (lambda (x) ((compose f f) x)) (- n 1))))
; ((repeated (lambda (x) (* x x)) 2) 5) => 625
