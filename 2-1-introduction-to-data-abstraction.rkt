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
