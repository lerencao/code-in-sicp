#lang planet neil/sicp

;;; half-interval method to find roots of equation f(x)=0 where
;;; is a continuous function.

(define (search f neg-point pos-point)
  (define (average x y) (/ (+ x y) 2))
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Value are not of opposite sign" a b)))))

; (half-interval-method sin 2.0 4.0)
; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


;;; fixed point of functions

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next-guess (f guess)))
      (if (close-enough? next-guess guess)
          next-guess
          (try next-guess))))
  (try first-guess))

; (fixed-point cos 1.0)
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)



;;; sqrt ...
;;; average damping
(define (sqrt x)
  (define (average x y) (/ (+ x y) 2.0))
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; exercise 1.35
;;; f(x) = 1+ 1/x = x
;;; equavalent to x^2 - x - 1 = 0, whose root are golden ratio
(define (golden-ratio)
  (fixed-point (lambda (x) (+ (/ 1.0 x) 1)) 1))

;;; exercise 1.36
;;; start with 10
(define (log-solution)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 10))
(define (log-solution-with-ad)
  (fixed-point
   (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0))
   10))
;;; with average damping, the steps is decreased.