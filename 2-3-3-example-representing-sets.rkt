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

; sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (entry set) x) true)
        ((< (entry set) x) (element-of-set? x (right-branch set)))
        ((> (entry set) x) (element-of-set? x (left-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-set x (right-branch set))))))

; exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree) result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

; the two procedures produce the same result. (a)
; list for figure 2.16 is (1 3 5 7 9 11)
; and they have same order of growth. (b)

; exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0) (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
; a
; select first (n-1)/2 elems to construct left tree,
; use the left to construct entry and right tree.

; b, lg(n)

; exercise 2.65
(define (union-set seta setb)
  (cond ((null? seta) setb)
        ((null? setb) seta)
        (else (let ((lsta (tree->list-1 seta))
                    (lstb (tree->list-1 setb)))
                ; use union-set for ordered list
                (let ((unioned-list (union-set lsta lstb)))
                  (list->tree unioned-list))))))
(define (intersection-set seta setb)
  (cond ((or (null? seta) (null? setb)) nil)
        (else (let ((lsta (tree->list-1 seta))
                    (lstb (tree->list-1 setb)))
                ; use intersection-set for ordered list
                (let ((intersectioned-list (intersection-set lsta lstb)))
                  (list->tree intersectioned-list))))))

; sets and information retrieval
; lookup for unordered set
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; lookup for ordered set
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((< given-key (key (car set-of-records))) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (cdr set-of-records)))))

; lookup for binary tree set
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
