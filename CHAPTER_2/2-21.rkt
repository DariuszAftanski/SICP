#lang racket

(require "Helper.rkt")

(
  define (square-list-1 items)
    (
      if (null? items)
         null
         (cons (* (car items) (car items))
               (square-list-1 (cdr items)))
    )
)

(
  define (square-list-2 items)
    (map (lambda (x) (square x)) items)
)

; TEST CASES:
;   square-list-1
(display "(square-list-1 (list 1 2 3 4)): ") (square-list-1 (list 1 2 3 4))
;   square-list-2
(display "(square-list-2 (list 1 2 3 4)): ") (square-list-2 (list 1 2 3 4))

