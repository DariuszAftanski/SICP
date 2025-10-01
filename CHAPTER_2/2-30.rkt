#lang racket

(require "Helper.rkt")

#|
Exercise 2.30:
Define a procedure square-tree analogous to the square-list procedure of
Exercise 2.21. That is, square-tree should behave as follows:
|#

(
  define (square-tree-1 tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree-1 (car tree))
                      (square-tree-1 (cdr tree)))))
)

(
  define (square-tree-2 tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-2 sub-tree)
                (square sub-tree)))
          tree)
)

(
  define (tree? object)
    (pair? object)
)

(
  define (square-tree-3 tree)
    (map (lambda (sub-tree)
            (if (tree? sub-tree)
                (square-tree-3 sub-tree)
                (square sub-tree)))
          tree)
)

; Test
(
  define (test square-tree-impl)
    (define test-list
      (list 1 (list 2 (list 3 4) 5) (list 6 7)))

    (display "Input           = ")(display test-list)(newline)
    (display "Actual result   = ")(display (square-tree-impl test-list))(newline)
    (display "Expected result = (1 (4 (9 16) 25) (36 49))")(newline)
)

