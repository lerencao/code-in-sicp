#lang planet neil/sicp


;(define (integral f a b dx)
;  (* (sum f
;          (+ a (/ dx 2.0))
;          (lambda (x) (+ x dx))
;          b)
;     dx))

;;; using lambda to create local variables
;;; f(x, y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
;;; a = 1+xy
;;; b = 1-y
;;; f(x, y) = xa^2 + yb + ab

(define (f-lambda x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ (* x y) 1)
   (- 1 y)))

;;; a special form called let to make it convenient
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))