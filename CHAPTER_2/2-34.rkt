#lang racket

(require "Helper.rkt")

#|
Exercise 2.34:
|#

; "accumulate" definition taken from the textbook.
(
  define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))
)

(
  define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                  (+ this-coeff (* higher-terms x)))
                0
                coefficient-sequence)
)

(
  define (test-horner-eval)
    (display "Testing 'horner-eval'...")
    (newline)

    (display "ACTUAL:    ")(display (horner-eval 2 (list 1 3 0 5 0 1)))
    (newline)
    (display "EXPECTED:  ")(display "79")
    (newline)
)

