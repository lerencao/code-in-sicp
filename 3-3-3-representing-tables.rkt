#lang planet neil/sicp

(define (make-table) (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key2 value) (cdr subtable)))))
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))))


(define (lookup key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record (cdr record) false))
        false)))
