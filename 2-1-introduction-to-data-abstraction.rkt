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
