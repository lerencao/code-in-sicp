#lang racket


(provide the-empty-stream stream-map-extended)

(define the-empty-stream
  empty-stream)
(define (stream-map-extended proc . argstreams)
  (if (stream-empty? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map-extended
              (cons proc
                    (map stream-rest
                         argstreams))))))
