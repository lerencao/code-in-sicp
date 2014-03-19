#lang planet neil/sicp

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)
(list 'a b)
(list 'a 1)

(list 'car (list 'quote '(a b c)))

; eq? can be used to test whether two symbols are the same.
(define (memq item seq)
  (cond ((null? seq) false)
        ((eq? item (car seq)) seq)
        (else (memq item (cdr seq)))))
; (memq 'apple '(pear banana prune))
