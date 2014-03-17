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
