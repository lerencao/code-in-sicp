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

; use ordered list to represent set
(define (element-of-set? e set)
  (cond ((null? set) false)
        ((= e (car set)) true)
        ((< e (car set)) false)
        (else (element-of-set? e (cdr set)))))
(define (intersection-set seta setb)
  (if (or (null? seta) (null? setb)) nil
      (let ((x1 (car seta))
            (x2 (car setb)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr seta) (cdr setb))))
              ((< x1 x2) (intersection-set (cdr seta) setb))
              ((> x1 x2) (intersection-set seta (cdr setb)))))))
(define (adjoin-set e set)
  (cond ((null? set) (list e))
        ((= e (car set)) set)
        ((< e (car set)) (cons e set))
        (else (cons (car set) (adjoin-set e (cdr set))))))
(define (union-set seta setb)
  (cond ((null? seta) setb)
        ((null? setb) seta)
        (else
         (let ((x1 (car seta)) (x2 (car setb)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr seta) (cdr setb))))
                 ((< x1 x2) (cons x1 (union-set (cdr seta) setb)))
                 (else (cons x2 (union-set seta (cdr setb)))))))))
