#lang racket

(require "Helper.rkt")
(require "2-38.rkt")

(provide reverse_1)
(provide reverse_2)

;
; Exercise 2.39:
; Complete the following deﬁnitions of reverse (Exercise 2.18) in terms of
; fold-right and fold-left from Exercise 2.38:
;

(
  define (reverse_1 sequence)
    (fold-right (lambda (x y) (append y (list x))) null sequence)
)

(
  define (reverse_2 sequence)
    (fold-left (lambda (x y) (append (list y) x ) ) null sequence)
)

