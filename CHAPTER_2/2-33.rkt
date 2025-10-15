#lang racket

(require "Helper.rkt")

#|
Exercise 2.33:
Fill in the missing expressions to complete the following deﬁnitions of some
basic list-manipulation operations as accumulations:
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
  define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) null sequence)
)

(
  define (test-map)
    (display "Testing 'map'...")
    (newline)

    (display "ACTUAL:    ")(display (map (lambda (x) (+ x 1)) (list 1 2 4 7)))
    (newline)
    (display "EXPECTED:  ")(display "(2 3 5 8)")
    (newline)

    (display "ACTUAL:    ")(display (map square (list 1 2 3 4 5)))
    (newline)
    (display "EXPECTED:  ")(display "(1 4 9 16 25)")
    (newline)
)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(
  define (test-append)
    (display "Testing 'append'...")
    (newline)

    (display "ACTUAL:    ")(display (append (list 1 2 3 4 5) (list 2 4 7)))
    (newline)
    (display "EXPECTED:  ")(display "(1 2 3 4 5 2 4 7)")
    (newline)
)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(
  define (test-length)
    (display "Testing 'length'...")
    (newline)

    (display "ACTUAL:    ")(display (length (list 9651 22 33 45 51)))
    (newline)
    (display "EXPECTED:  ")(display "5")
    (newline)
)
