#lang planet neil/sicp

;;; the basic idea of data abstraction is to structure the programs that
;;; are to use compound data objects so that they operate on "abstract data".
;;;That is, our programs should use data in such a way as to make no assumptions
;;; about the data that are not strictly necessary for performing the task
;;; at hand. At the same time, a "concrete" data representation is defined
;;; independent of the programs that use the data. The interface between these
;;; two parts of our system will be a set of procedures, called selectors and
;;; constructors, that implement the abstract data in terms of
;;; the concrete representation.


;;; 2.1.1 Example: Arithmetic Operations for Rational Numbers

; constructors: (make-rat <n> <d>)
; selectors: (numer <x>), (denom (x))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; pair constructors and selectors: cons, car, cdr
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
; or this, it is efficient, but defeats debugging.
; (define make-rat cons)
; (define numer car)
; (define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; now you can try it.
; (print-rat (add-rat (make-rat 1 2) (make-rat 1 3)))

;; reduce to lowest terms using gcd
(define (make-rat-reduced n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; exercise 2.1
(define (make-rat-better n d)
  (let ((g (gcd (abs n) (abs d))))
    (let ((ln (/ n g))
          (ld (/ d g)))
      (if (< (* ln ld) 0)
          (make-rat (- (abs ln)) (abs ld))
          (make-rat (abs ln) (abs ld))))))

;;; 2.1.2 Abstraction Barriers
(define (make-rat-abs n d) (cons n d))
(define (numer-abs x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom-abs x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; exercise 2.2
(define (make-segment x y) (cons x y))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment line)
  (let ((x1 (x-point (start-segment line)))
        (y1 (y-point (start-segment line)))
        (x2 (x-point (end-segment line)))
        (y2 (y-point (end-segment line))))
    (make-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; exercise 2.3

(define (perimeter rectangle)
  (* (+ (length rectangle) (width rectangle)) 2))
(define (area rectangle)
  (* (length rectangle) (width rectangle)))

(define (mk-rectangle l-seg w-seg)
  (cons l-seg w-seg))
(define (length rectangle)
  (let ((a (start-segment (car rectangle)))
        (b (end-segment (car rectangle))))
    (- (x-point b) (x-point a))))
(define (width rectangle)
  (let ((a (start-segment (cdr rectangle)))
        (d (end-segment (cdr rectangle))))
    (- (y-point d) (y-point a))))


;; 2.1.3 What is meant by data
; procedural representations of data and message passing
(define (cons-alt x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
    dispatch)
(define (car-alt z) (z 0))
(define (cdr-alt z) (z 1))

;; exercise 2.4
(define (cons-aalt x y) (lambda (m) (m x y)))
(define (car-aalt z) (z (lambda (p q) p)))
(define (cdr-aalt z) (z (lambda (p q) q)))

;; exercise 2.5
(define (int-cons a b) (* (expt 2 a) (expt 3 b)))
(define (int-car z)
  (if (not (= (remainder z 2) 0)) 0
      (+ 1 (int-car (/ z 2)))))
(define (int-cdr z)
  (if (not (= (remainder z 3) 0)) 0
      (+ 1 (int-cdr (/ z 3)))))

;; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;; 2.1.4 Extended Exercise: Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((a1 (lower-bound x))
        (b1 (upper-bound x))
        (a2 (lower-bound y))
        (b2 (upper-bound y)))
    (cond ((and (< b1 0)
                (< b2 0))
           (make-interval (* b1 b2) (* a1 a2)))
          ((and (< b1 0)
                (> a2 0))
           (make-interval (* a1 b2) (* b1 a2)))
          ((and (< b1 0)
                (<= a2 0) (>= b2 0))
           (make-interval (* a1 b2) (* a1 a2)))

          ((and (<= a1 0) (>= b1 0)
                (< b2 0))
           (make-interval (* b1 a2) (* a1 a2)))
          ((and (<= a1 0) (>= b1 0)
                (> a2 0))
           (make-interval (* a1 b2) (* b1 b2)))
          ((and (<= a1 0) (>= b1 0)
                (<= a2 0) (>= b2 0))
           (let ((p1 (* a1 b2))
                 (p2 (* a2 b1))
                 (p3 (* a1 a2 ))
                 (p4 (* b1 b2 )))
             (make-interval (min p1 p2) (max p3 p4))))

          ((and (> a1 0)
                (< b2 0))
           (make-interval (* b1 a2) (* a1 b2)))
          ((and (> a1 0)
                (> a2 0))
           (make-interval (* a1 a2) (* b1 b2)))
          ((and (> a1 0)
                (<= a2 0) (>= b2 0))
           (make-interval (* b1 a2) (* b1 b2))))))
(define (div-interval x y)
  (if (= (upper-bound y) (lower-bound y))
      (error "interval spans zero")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

;; exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; exercise 2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (upper-bound y)))
        (p2 (- (upper-bound x) (lower-bound y))))
    (make-interval p1 p2)))

;; exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
(define (width-sum x y)
  (+ (width-interval x) (width-interval y)))
(define width-sub width-sum)

;; exercise 2.10
; see the definition of div-interval

;; exercise 2.11
; see the definition of mul-interval

;; exercise 2.12
(define (make-center-percent center percentage)
  (make-interval (* center (+ 1 percentage))
                 (* center (- 1 percentage))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (percent i)
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0))
  (/ width center))

;; exercise 2.13
; p = (p1+p2)/(1+p1p2)

;; exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval
                       (div-interval one r1)
                       (div-interval one r2)))))
; run the follows two expressions, and get different results
; (par1 (make-interval 10 15) (make-interval 15 20))
; (par2 (make-interval 10 15) (make-interval 15 20))

(define A (make-center-percent 10 0.001))
(define B (make-center-percent 20 0.002))
(par1 A B)
(par2 A B)

;; exercise 2.15
; because an uncertain elem will be amplified if repeated
