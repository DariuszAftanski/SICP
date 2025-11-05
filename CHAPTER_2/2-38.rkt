#lang racket

(require "Helper.rkt")

(provide fold-left)
(provide fold-right)

;
; Exercise 2.38:
; The accumulate procedure is also known as fold-right, because it combines the
; first element of the sequence with the result of combining all the elements to
; the right. There is also a fold-left, which is similar to fold-right, except
; that it combines elements working in the opposite direction:
;

;
; Declaration of "accumulate" taken from the textbook.
;
(
  define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))
)

;
; Declaration of "fold-left" taken from the textbook.
; It performs "op" on elements of the sequence starting from the left.
;
(
  define (fold-left op initial sequence)
    (
      define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest)))
    )
    (iter initial sequence)
)

;
; Declaration of "fold-right" - copy of "accumulate".
; It iterates recursively through the sequence until it gets to the last
; element. At this point, it starts performing the "op" on elements of the
; sequence starting from the right.
;
(
  define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence))))
)

;
; What are the values of:
;

; (fold-right / 1 (list 1 2 3))
;   3/2
; (fold-left / 1 (list 1 2 3))
;   1/6
; (fold-right list nil (list 1 2 3))
;   '(1 (2 (3 ())))
; (fold-left list nil (list 1 2 3))
;   '(((() 1) 2) 3)

;
; Give a property that op should satisfy to guarantee that fold-right and
; fold-left will produce the same values for any sequence.
;
; ----> "+"
