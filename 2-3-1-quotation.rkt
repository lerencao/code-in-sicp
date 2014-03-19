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

;; exercise 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
