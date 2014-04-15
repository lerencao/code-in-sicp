#lang planet neil/sicp

(define (make-table) '(*table* ()))

(define (assoc key records)
  (cond [(null? records) false]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

(define (look-up key table)
  (let ((record (assoc key (cadr table))))
    (if record
        (let ((value (cddr record)))
          (if (pair? value)
              (car value)
              false))
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cadr table))))
    (if record
        (set-cdr! (cdr record) (list value))
        (set-car! (cdr table) (cons (list key '() value) (cadr table))))))

(define (lookup-multi key-list table)
  (cond ((null? key-list) (error "lookup-multi called with empty list"))
        ((null? (cdr key-list))
         (look-up (car key-list) table))
        (else
         (let ((subtable (assoc (car key-list) (cadr table))))
           (if subtable
               (lookup-multi (cdr key-list) subtable)
               false)))))

(define (insert-multi! key-list value table)
  (cond [(null? key-list)
         (error "insert-multi! called with empty key list")]
        [(null? (cdr key-list))
         (insert! (car key-list) value table)]
        [else
         (let ((subtable (assoc (car key-list) (cadr table))))
           (if subtable
               (insert-multi! (cdr key-list) value subtable)
               (begin
                 (set-car! (cdr table) (cons (list (car key-list) '()) (cadr table)))
                 (set! subtable (assoc (car key-list) (cadr table)))
                 (insert-multi! (cdr key-list) value subtable))))]))
