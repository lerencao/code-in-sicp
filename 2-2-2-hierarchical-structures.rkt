#lang planet neil/sicp

;;; 2.2.2 hierarchical structures
(define (count-leaves ls)
  (cond ((null? ls) 0)
        ((not (pair? ls)) 1)
        (else (+ (count-leaves (car ls))
                 (count-leaves (cdr ls))))))
; (count-leaves (list 1 2 (list 2 3)))

;; exercise 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr
      (car (cdr
            (car (cdr
                  (car (cdr
                        (car (cdr
                              (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; exercise 2.26
'(1 2 3 4 5 6)
'((1 2 3) 4 5 6)
'((1 2 3) (4 5 6))

;; exercise 2.27
(define (deep-reverse ls)
  (cond ((null? ls) nil)
        ((pair? (car ls))
         (append (deep-reverse (cdr ls))
                 (deep-reverse (car ls))))
        (else (append (deep-reverse (cdr ls))
                      (list (car ls))))))
; (deep-reverse (list 1 2 3))
; (deep-reverse '(1 (2 3) ((4 5) 6)))

;; exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))
; (fringe '((1 2) (3 4)))
; (fringe '(((1 2) (3 4)) ((1 2) (3 4))))

;; exercise 2.29

(define (make-mobile left right) (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (make-branch length structure) (list length structure))
(define (branch-length br) (car br))
(define (branch-structure br) (cadr br))

(define (total-weight mobile)
  (if (null? mobile) mobile
      (+ (total-weight (left-branch mobile))
         (total-weight (right-branch mobile)))))

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))
(define (balanced? mobile)
  (cond ((not (pair? mobile)) true)
        ((and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
              (balanced? (branch-structure (left-branch mobile)))
              (balanced? (branch-structure (right-branch mobile))))
         true)
        (else false)))

;; mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
; (scale-tree '(1 (2 3) (4 5) 2) 2)

(define (scale-tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree proc)
             (proc sub-tree)))
       tree))
; (scale-tree-map (lambda (x) (* x 2)) '(1 (2 3) (4 5) 2))

;; exercise 2.30
(define (direct-square-tree tree)
  (scale-tree tree 2))
;(direct-square-tree '((1 2) (3 (4 5)) 6))
(define (map-square-tree tree)
  (scale-tree-map tree (lambda (x) (* x 2))))
;(map-square-tree '((1 2) (3 (4 5)) 6))

;; exercise 2.32
(define (subsets s)
  (if (null? s) (list nil)
      (let ((left (subsets (cdr s))))
        (append left (map (lambda (ls) (cons (car s) ls)) left)))))
;(subsets '(1 2 3))
