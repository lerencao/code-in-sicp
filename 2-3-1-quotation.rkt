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

;; exercise 2.54
(define (equal? x y)
  (cond ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (list? x) (list? y))
         (cond ((and (null? x) (null? y)) true)
               ((or (null? x) (null? y)) false)
               (else (and (equal? (car x) (car y))
                          (equal? (cdr x) (cdr y))))))))

;; exercise 2.54
; the quotation is just a single-character abbr for wrapping the next complete expression with quote to form (quote <expression>).
; ''asd => (quote (quote asd))
