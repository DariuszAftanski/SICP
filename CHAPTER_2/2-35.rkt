#lang racket

(require "Helper.rkt")

#|
Exercise 2.35:
Redefine count-leaves from Section 2.2.2 as an accumulation:
|#

; "accumulate" definition taken from the textbook.
(
  define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))
)

; "count-leaves" definition taken from textbook: 2.2.2
(
  define (count-leaves-1 x)
    (cond
      ((null? x) 0)
      ((not (pair? x)) 1)
      (else (+ (count-leaves-1 (car x))
        (count-leaves-1 (cdr x)))))
)

(define (count-leaves-2 tree)
  (accumulate + 0 (map (lambda (x) (if (pair? x)(count-leaves-2 x) 1)) tree)))

(
  define (test-count-leaves)
    (display "Testing 'count-leaves'...")
    (newline)

    (define test-set
      (list 1 (list 2 (list 3 4) 5) (list 6 7)))

    (display "ACTUAL:    ")(display (count-leaves-1 test-set))
    (newline)
    (display "EXPECTED:  ")(display (count-leaves-2 test-set))
    (newline)
)

