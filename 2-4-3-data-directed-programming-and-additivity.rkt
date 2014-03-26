#lang planet neil/sicp

; dispatching on type: checking the type of datum and calling an appropriate procedure.
; But, it has two weaknesses.
; 1. the generic interface must know all about the different representations.
; 2. We must guarantee that no two procedures in the entire system have the same name.
; The reason is that this technique is not *additive*.
; The solution is the programming technique known as *data-directed programming*.

; Assume we have two procedures.
; 1. (put <op> <type> <item>)
; 2. (get <op> <type>)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr  z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  ;; interface to the rest of system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these typs: APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a, they has no operands

; b
(define (install-sum-package)
  (define (deriv-sum operands var)
    (+ (deriv (car operands) var)
       (deriv (cadr operands) var)))
  (put 'deriv '+ deriv-sum)
  'done)
(define (install-product-package)
  (define (deriv-product operands var)
    (let ((left (car operands))
          (right (cadr operands)))
      (+ (* (deriv left) right)
         (* (deriv right) left))))
  (put 'deriv '* deriv-product))

; c

(define (install-exponent-package)
  (define (deriv-exponent operands var)
    (let ((base (car operands))
          (exp (cadr operands)))
      (* exp ('** base (- exp 1)) (deriv base var))))
  (put 'deriv '** deriv-exponent)
  'done)

; d
; install one package, implement all operators in the package.


; exercise 2.74
(define (install-division-package)
  (define (get-record employee-name personnel-records)
    (cons 'division-name '(self implementation)))
  (put 'get-record 'division-name get-record)
  (define (get-salary record) (car record))
  (put 'get-salary 'division-name get-salary)
  'done)

(define (get-record employee-name persionnel-file)
  ((get 'get-record (car persionnel-records)) employee-name (cadr personnel-records)))

(define (get-salary employee-record)
  ((get 'get-salary (car employee-record)) (cadr employee-record)))

;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unkown op: make-from-real-imag" op))))
  dispatch)
; the value returned by make-from-real-imag is a procedure.
(define (apply-generic op arg) (arg op))

; exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unkown op: make-from-mag-ang" op))))
  dispatch)

; exercise 2.76

; select generic operations with explicit dispatch
; when new operations must often be added.

; and select Message passing when new types must often be added.
