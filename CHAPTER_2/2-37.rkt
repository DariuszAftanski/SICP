#lang racket

(require "Helper.rkt")

(provide dot-product)
(provide matrix-*-vector)
(provide transpose)
(provide matrix-*-matrix)

#|
Exercise 2.37:
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

#|
This dot-product implementation uses the extended version of map.
The more general map takes a procedure of n arguments, together with n lists,
and applies the procedure to all the first elements of the lists, all the second
elements of the lists, and so on, returning a list of the results.

For example:

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

(741 852 963)

|#
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose matrix)
  (if (null? matrix)
      null
      (accumulate-n cons null matrix)))

(define (matrix-*-matrix m n)
  (let ([transposed-n (transpose n)])
  (map (lambda (row) (matrix-*-vector transposed-n row)) m)))

