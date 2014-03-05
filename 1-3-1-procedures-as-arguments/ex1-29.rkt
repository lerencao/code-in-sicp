#lang racket

;;; Simpson's Rule to compute numerical integration

; common pattern of abstraction
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define (h) (/ (- b a) n))
  (define (hfn) (/ n 2))

  ;; how to calculate yk
  (define (term-y k)
    (define (c k)
      (cond ((or (= k 0) (= k (hfn))) 1)
            ((< k (hfn))
             (if (= (remainder k 2) 1)
                 4
                 2))
            (else (c (- n k)))))
    (* (c k) (f (+ a (* k (h))))))

  (* (/ (h) 3.0)
     (sum term-y 0 (lambda (k) (+ k 1)) n)))
