#lang racket

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (add-streams s1 s2)
  (stream-map-extended + s1 s2))
(define (stream-map-extended proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map-extended
              (cons proc
                    (map stream-rest
                         argstreams))))))



(provide scale-stream add-streams)
