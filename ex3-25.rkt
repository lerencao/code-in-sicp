#lang planet neil/sicp

(define (make-table) (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))

(define (insert-multi! key-list value table)
  (cond ((null? key-list)
         (error "INSERT-MULTI! called with empty key list" key-list))
        ((null? (cdr key-list))
         (insert! (car key-list) value table))
        (else
         (let ((subtable (assoc (car key-list) (cdr table))))
           (if subtable
               (insert-multi! (cdr key-list) value subtable)
               (begin
                 (set-cdr! table
                           (cons (list (car key-list))
                                 (cdr table)))
                 (set! subtable (assoc (car key-list) (cdr table)))
                 (insert-multi! (cdr key-list) value subtable)))))))

(define (lookup-multi key-list table)
  (cond ((null? key-list)
         (error "LOOKUP called with empty key list" key-list))
        ((null? (cdr key-list))
         (lookup (car key-list) table))
        (else
         (let ((subtable (assoc (car key-list) (cdr table))))
           (if (or (not subtable) ; cannot find the key
                   (not (pair? (cdr subtable)))) ; or have no subtables for left keys
               false
               (lookup-multi (cdr key-list) subtable))))))
