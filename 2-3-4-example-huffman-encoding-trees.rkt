#lang planet neil/sicp

;;; 2.3.4 Example: Huffman encoding trees
; first, knowledge about the Huffman Encoding

; ops on leaf node ('leaf symbol weight)
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))
(define (leaf? object) (equal? (car object) 'leaf))

; ops on tree node (left right symbols weight)
(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

; decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) nil
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit current-branch)
  (cond ((= bit 1) (right-branch current-branch))
        ((= bit 0) (left-branch current-branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; sets of weighted elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; the result is (a d a b b c a)

; exercise 2.68
(define (encode msg tree)
  (if (null? msg) nil
      (append (encode-symbol (car msg) tree)
              (encode (cdr msg) tree))))
(define (encode-symbol sym tree)
  (cond ((pair? (member sym (symbols (left-branch tree))))
         (if (leaf? (left-branch tree))
             '(0)
             (cons 0 (encode-symbol sym (left-branch tree)))))
        ((pair? (member sym (symbols (right-branch tree))))
         (if (leaf? (right-branch tree))
             '(1)
             (cons 1 (encode-symbol sym (right-branch tree)))))
        (else (error "bad symbol: ENCODE-SYMBOL" sym))))
; (display (encode '(a d a  b b c a) sample-tree))

; exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge node-set)
  (cond ((null? node-set) (error "empty node set: successive-merge"))
        ((null? (cdr node-set)) (car node-set))
        (else (successive-merge
               (adjoin-set (make-code-tree (car node-set) (cadr node-set))
                           (cdr (cdr node-set)))))))

; exercse 2.70
(define rock-songs-pairs '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define code-tree (generate-huffman-tree rock-songs-pairs))
(define msg
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))
; (display (encode msg code-tree)) => length = 84
; if we use fixed-length code, the length will be 3 * (3+9+3+9+10+2) = 108

; exercise 2.71
; for the most frequent symbol, bits = 1
; for the least frequent symbol, bits = n - 1
