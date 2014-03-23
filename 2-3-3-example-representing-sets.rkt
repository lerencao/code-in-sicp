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

; exercise 2.60
; change adjoin-set to the follows, and others remain the same.
; (define (adjoin-set e set)
  ; (cons e set))
; as for efficiency, all but the adjoin-set is worse.
; multiset can be used here for duplicate elements.
; application: the multiset of prime factors of a number n.
; for number 120, the result is {2, 2, 2, 3, 5}
