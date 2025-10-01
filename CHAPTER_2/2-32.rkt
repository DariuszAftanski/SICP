#lang racket

(require "Helper.rkt")

#|
Exercise 2.32:
We can represent a set as a list of distinct elements, and we can represent the
set of all subsets of the set as a list of lists.

For example, if the set is (1 2 3),
then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).

Complete the following definition of a procedure that generates the set of
subsets of a set and give a clear explanation of why it works:
|#

(
  define (tree? object)
    (pair? object)
)

(
  define (subsets s)
    (display "s: ")(display s)(newline)
    (if (null? s)
        (list null)
        (let ((rest (subsets (cdr s))))
          (display "rest: ")(display rest)(newline)
          (append rest (map (lambda (x) (cons (car s) x)) rest))))
)

; Test
(
  define (test)
    (subsets (list 1 2 3))
)

; DONE BY TRIAL AND ERROR. Need to analyse
