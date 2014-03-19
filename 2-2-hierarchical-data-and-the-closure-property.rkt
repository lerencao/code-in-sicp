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

; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
; two conditions.
; 1. op is a monoid.
; 2. op(init, a) = op(a, init).

; exercise 2.39
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;; nest mapping
;(define (pairs n)
;  (accumulate append nil
;              (map (lambda (i)
;                     (map (lambda (j) (list j i))
;                          (enumerate-interval 1 (- i 1))))
;                   (enumerate-interval 1 n))))

(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))

(define (pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (odd-sum? pair)
  (odd? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (odd-sum-pairs n)
  (map make-pair-sum
       (filter odd-sum? (pairs n))))

(define (permutations seq)
  (if (null? seq) (list nil)
      (flat-map (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x seq))))
                seq)))
(define (remove x s)
  (filter (lambda (e) (not (= e x))) s))

;; exercise 2.40
(define unique-pairs pairs)

;; exercise 2.41
(define (triples n)
  (flat-map (lambda (i)
              (flat-map (lambda (j)
                          (map (lambda (k) (list i j k))
                               (enumerate-interval (+ j 1) n)))
                        (enumerate-interval (+ i 1) n)))
            (enumerate-interval 1 n)))
; another way, use pairs for simplicity
(define (triples-e n)
  (flat-map (lambda (p)
              (map (lambda (k) (cons k p))
                   (enumerate-interval (+ (car p) 1) n)))
            (pairs n)))

(define (sum-triples n s)
  (define (sum-equal? seq)
    (let ((acc (accumulate + 0 seq)))
      (= s acc)))
  (filter sum-equal? (triples-e n)))

;; exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flat-map (lambda (rest-of-queens)
                            (map (lambda (new-row)
                                   (adjoin-position new-row k rest-of-queens))
                                 (enumerate-interval 1 board-size)))
                          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)
(define (adjoin-position row k rest-of-queens)
  (cons (list k row) rest-of-queens))
(define (safe? k positions)
  (define (safe-with? q1 q2) ; test whether two queens satisfy the three conditions
    (cond ((= (car q1) (car q2)) false) ; same column
          ((= (cadr q1) (cadr q2)) false) ; same row
          ((= (abs (- (car q1) (car q2)))
              (abs (- (cadr q1) (cadr q2))))
           false) ; diagonal
          (else true)))
  (let ((to-check (find-first (lambda (p) (= (car p) k)) positions)))
    (let ((checked (remove-all (lambda (p) (= (car p) k)) positions)))
      (all? (lambda (p) (safe-with? to-check p)) checked))))

; helper methods for eight-queens
(define (find-first pred seq) ; find first elem who satisfies pred
  (if (pred (car seq)) (car seq)
      (find-first pred (cdr seq))))
(define (remove-all pred seq) ; remove all elems who satisfy pred
  (cond ((null? seq) seq)
        ((pred (car seq)) (remove-all pred (cdr seq)))
        (else (cons (car seq) (remove-all pred (cdr seq))))))
(define (all? pred seq) ; test whether all elems in seq satisfy pred
  (if (null? (filter (lambda (e) (not (pred e))) seq))
      true
      false))

;; exercise 2.43
; queen-cols would be called repeatedly, which causes too much computation.
; it will take T*k^k.
