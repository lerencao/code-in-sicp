#lang planet neil/sicp

;;; representing sets

; operations on sets
(define (union-set seta setb)
  (cond ((null? seta) setb)
        ((element-of-set? (car seta) setb)
         (union-set (cdr seta) setb))
        (else (cons (car seta) (union-set (cdr seta) setb)))))
(define (intersection-set seta setb)
  (cond ((null? seta) nil)
        ((element-of-set? (car seta) setb)
         (cons (car seta) (intersection-set (cdr seta) setb)))
        (else (intersection-set (cdr seta) setb))))
(define (element-of-set? e set)
  (cond ((null? set) false)
        ((equal? e (car set)) true)
        (else (element-of-set? e (cdr set)))))
(define (adjoin-set e set)
  (if (element-of-set? e set) set (cons e set)))
