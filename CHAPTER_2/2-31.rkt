#lang racket

(require "Helper.rkt")

#|
Exercise 2.31:
Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the
property that square-tree could be defined as
|#


(
  define (tree? object)
    (pair? object)
)

(
  define (tree-map func tree)
    (map (lambda (sub-tree)
            (if (tree? sub-tree)
                (tree-map func sub-tree)
                (func sub-tree)))
          tree)
)

(
  define (square-tree tree)
    (tree-map square tree)
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

