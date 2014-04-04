#lang racket

(define rand
  (let ((x random-init))
    (lambda (signal)
      (cond ((eq? 'generate) (set! x random-init))
            ((eq? 'reset) (lambda (new-value) (set! x new-value)))
            (else (set! x (rand-update x))))
      x)))

;; (define (estimate-pi trials)
;;   (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;; (define (cesaro-test)
;;   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment) (p (random-in-range x1 x2)
                          (random-in-range y1 y2)))
  (let ((rate (monte-carlo trials experiment))
        (area (* (- x2 x1) (- y2 y1))))
    (* rate area)))

(define (estimate-pi trials)
  (estimate-integral (lambda (x y) (<= (+ (expt x 2) (expt y 2)) 1))
                          -1 1 -1 1
                          trials))
