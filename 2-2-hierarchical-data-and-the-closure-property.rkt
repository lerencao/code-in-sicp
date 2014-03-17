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
(define (map proc ls)
  (if (null? ls) nil
      (cons (proc (car ls))
            (map proc (cdr ls))))) ;(map (lambda (x) (* x 2)) (list 1 2 3))

;;exercise 2.21
(define (square-list ls)
  (if (null? ls) nil
      (cons (* (car ls) (car ls))
            (square-list (cdr ls)))))
(define (square-list-map ls)
  (map (lambda (x) (* x x)) ls))
; (square-list-map '(1 2 3))
