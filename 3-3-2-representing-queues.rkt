#lang planet neil/sicp

;;; interface for queue
; make-queue
; empty-queue?
; front-queue
; insert-queue!
; delete-queue!


;;; concrete representation
(define (make-queue) (cons '() '()))

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (empty-queue? q) (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with an empty queue" q))
         (else
          (set-front-ptr! q (cdr (front-ptr q)))
          q)))

;;; exercise 3.21

(define (print-queue q) (display (front-ptr q)))

;;; exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-car! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE called with an empty queue")
          (set! front-ptr (cdr front-ptr))))
    (define (print)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print))))
    dispatch))

(define (empty-queue? q)
  (q 'empty-queue?))
(define (front-queue q)
  (q 'front-queue))
(define (insert-queue! q item)
  ((q 'insert-queue!) item))
(define (delete-queue! q)
  (q 'delete-queue!))
(define (print-queue q)
  (q 'print-queue))
