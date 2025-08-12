#lang racket

(require "Helper.rkt")

(
  define (square-list-1 items)
    (
      define (iter things answer)
        (
          if (null? things)
              answer
              (iter (cdr things)
                    (cons (square (car things)) answer))
        )
    )

    (iter items null)
)

; This produces a list in reversed order, because previously calculated elements
; are put at the end of the list

(
  define (square-list-2 items)
    (
      define (iter things answer)
        (
          if (null? things)
             answer
             (iter (cdr things)
                   (cons answer (square (car things))))
        )
    )

    (iter items null)
)

; answer is a list, not an integer. cons adds a list to a list - nesting.

; TEST-CASES
; square-list-1
(display "(square-list-1 (list 1 2 3 4)): ") (square-list-1 (list 1 2 3 4))
; square-list-2
(display "(square-list-2 (list 1 2 3 4)): ") (square-list-2 (list 1 2 3 4))

