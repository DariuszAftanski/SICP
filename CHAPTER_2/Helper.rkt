#lang racket

(provide average)
(provide square)

(define (average a b)
  (/ (+ a b) 2.0)
)

(define (square a)
  (* a a)
)
