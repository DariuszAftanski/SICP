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
  define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs))))
)

(
  define (test-accumulate-n)
    (display "Testing 'accumulate-n'...")
    (newline)

    (define test-set (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12) ))

    (display "ACTUAL:    ")(display (accumulate-n + 0 test-set))
    (newline)
    (display "EXPECTED:  ")(display "(22 26 30)")
    (newline)
)

