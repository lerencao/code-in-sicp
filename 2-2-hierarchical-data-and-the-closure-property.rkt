#lang planet neil/sicp

;;; closure property can form a hierarchical structure.

;;; 2.2.1 using pair to represent sequences

(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

(list 1 2 3 4)

(define one-four (list 1 2 3 4))

(car one-four)
(cdr one-four)
(cons 10 one-four)

;;; list operations
(define (list-ref ls n)
  (if (= n 0)
      (car ls)
      (list-ref (cdr ls) (- n 1)))) ; (list-ref one-four 2)

(define (length ls)
  (if (null? ls)
      0
      (+ 1 (length (cdr ls))))) ;(length '(1 3 5 7))
(define (length-iter ls)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ count 1))))
  (iter ls 0))

(define (append ls1 ls2)
  (if (null? ls1)
      ls2
      (cons (car ls1) (append (cdr ls1) ls2))))
