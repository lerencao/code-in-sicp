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

;; exercise 2.17
(define (last-pair ls)
  (cond ((null? ls) (error "empty list"))
        ((null? (cdr ls)) ls)
        (else (last-pair (cdr ls))))) ;(last-pair (list 1 2))

;; exercise 2.18
(define (reverse ls)
  (if (null? ls)
      nil
      (append (reverse (cdr ls))
              (list (car ls))))) ;(reverse (list 1 2))

;; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amounts coin-values)
  (define no-more? null?)
  (define first-denomination car)
  (define except-first-denomination cdr)
  (cond ((= amounts 0) 1)
        ((or (< amounts 0) (no-more? coin-values)) 0)
        (else
         (+ (cc (- amounts (first-denomination coin-values)) coin-values)
            (cc amounts (except-first-denomination coin-values))))))
; (cc 100 us-coins)
; (cc 100 (list 5 10 1 25 50))
; the order of coin list doesn't affect the output of function cc

; exercise 2.20
(define (same-parity l . ls)
  (define (iter elems res)
    (if (null? elems)
        res
        (if (= (remainder (- (car elems) l) 2) 0)
            (iter (cdr elems) (cons (car elems) res))
            (iter (cdr elems) res))))
  (reverse (iter ls (list l)))) ;(same-parity 1 2 3 4 5 6)

;;; map over list
;(define (map proc ls)
;  (if (null? ls) nil
;      (cons (proc (car ls))
;            (map proc (cdr ls))))) ;(map (lambda (x) (* x 2)) (list 1 2 3))

;;exercise 2.21
(define (square-list ls)
  (if (null? ls) nil
      (cons (* (car ls) (car ls))
            (square-list (cdr ls)))))
(define (square-list-map ls)
  (map (lambda (x) (* x x)) ls))
; (square-list-map '(1 2 3))

;; exercise 2.22
; wrong usage of form cons

; exercise 2.23
(define (for-each proc ls)
  (cond ((not (null? ls))
         (proc (car ls))
         (for-each proc (cdr ls)))))
; (for-each (lambda (x) (newline) (display x)) '(1 2 3))

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

; exercise 2.27
(define (deep-reverse ls)
  (cond ((null? ls) nil)
        ((pair? (car ls))
         (append (deep-reverse (cdr ls))
                 (deep-reverse (car ls))))
        (else (append (deep-reverse (cdr ls))
                      (list (car ls))))))
; (deep-reverse (list 1 2 3))
; (deep-reverse '(1 (2 3) ((4 5) 6)))

; exercise 2.28
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

; exercise 2.32
(define (subsets s)
  (if (null? s) (list nil)
      (let ((left (subsets (cdr s))))
        (append left (map (lambda (ls) (cons (car s) ls)) left)))))
;(subsets '(1 2 3))

;;; 2.2.3 sequences as conventional interfaces
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (* tree tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (define (iter a b k)
    (if (= k 0) a
        (iter (+ a b) a (- k 1))))
  (iter 0 1 n))
(define (even-fibs n)
  (define (iter k)
    (if (> k n) nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (iter (+ k 1)))
              (iter (+ k 1))))))
  (iter 0))

; use map, filter, accumulate to abstract the process
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (fringe tree)) ; use exercise 2.28
(define (sum-odd-squares-fn tree)
  (accumulate + 0 (map (lambda (x) (* x x))
                       (filter odd? (enumerate-tree tree)))))
(define (even-fibs-fn n)
  (accumulate cons nil (filter even?
                               (map fib (enumerate-interval 0 n)))))
;(sum-odd-squares-fn '(1 2 (3 4) (5 6)))
;(even-fibs-fnl 9)

;; exercise 2.33
(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
;(map-acc (lambda (x) (* x 2)) '(1 2 3))
(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))
;(append-acc '(1 2 3) '(4 5 6))
(define (length-acc sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
;(length-acc '(1 2 3))

; exercise 2.34
(define (horner-eval x coeffs)
  (accumulate (lambda (c mem) (+ c (* mem x)))
              0
              coeffs))
; (horner-eval 2 '(1 3 0 5 0 1))

; exercise 2.35
(define (count-leaves-acc t)
  (accumulate + 0
              (map
               (lambda (sub-tree)
                 (if (not (pair? sub-tree)) 1
                     (count-leaves-acc sub-tree)))
               t)))

; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (ls) (car ls)) seqs))
            (accumulate-n op init (map (lambda (ls) (cdr ls)) seqs)))))
; (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w))) ; the map here is racket version

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (matrix-*-matrix m n)
  (map (lambda (row)
         (map (lambda (col)
                (dot-product row col))
              (transpose n)))
       m))

(define (transpose m)
  (accumulate-n cons nil m))
;(define v '(1 2 3))
;(define w '(4 5 6))
;(dot-product v w)
;
;(define m '((1 2 3) (4 5 6)))
;(matrix-*-vector m v)
;
;(define n '((2 1) (2 1) (2 1)))
;(matrix-*-matrix m n)
;(transpose m)
