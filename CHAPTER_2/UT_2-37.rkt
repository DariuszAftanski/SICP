#lang racket

(require "2-37.rkt")

(require rackunit)
(require rackunit/text-ui)

(define dot-product-tests
  (test-suite "Tests for dot-product"

   (test-case "basic cases"
     (check-equal? (dot-product (list 1 2 3) (list 4 5 6))
                   32
                   "Should compute 1*4 + 2*5 + 3*6 = 32"))

   (test-case "empty vectors"
     (check-equal? (dot-product (list) (list))
                   0
                   "Dot product of empty vectors should be 0"))

   (test-case "single element"
     (check-equal? (dot-product (list 5) (list 3))
                   15
                   "Should compute 5*3 = 15"))

   (test-case "zero vector"
     (check-equal? (dot-product (list 0 0 0) (list 1 2 3))
                   0
                   "Dot product with zero vector should be 0"))

   (test-case "negative numbers"
     (check-equal? (dot-product (list 1 -2 3) (list 4 5 -6))
                   -24
                   "Should handle negative numbers: 1*4 + (-2)*5 + 3*(-6) = -24"))

   (test-case "floating point"
     (check-equal? (dot-product (list 1.5 2.0 3.5) (list 2.0 1.0 0.5))
                   6.75
                   "Should work with floating point: 1.5*2.0 + 2.0*1.0 + 3.5*0.5 = 6.75"))

   (test-case "orthogonal vectors"
     (check-equal? (dot-product (list 1 0) (list 0 1))
                   0
                   "Orthogonal vectors should have dot product 0"))

   (test-case "same vector"
     (check-equal? (dot-product (list 3 4) (list 3 4))
                   25
                   "Dot product of vector with itself: 3*3 + 4*4 = 25"))))

(define matrix-*-vector-tests
  (test-suite "Tests for matrix-*-vector"

   (test-case "2x2 matrix"
     (check-equal? (matrix-*-vector (list (list 1 2) (list 3 4)) (list 5 6))
                   (list 17 39)))

   (test-case "3x3 identity matrix"
     (check-equal? (matrix-*-vector (list (list 1 0 0) (list 0 1 0) (list 0 0 1)) (list 2 3 4))
                   (list 2 3 4)))

   (test-case "3x2 matrix"
     (check-equal? (matrix-*-vector (list (list 1 2) (list 3 4) (list 5 6)) (list 7 8))
                   (list 23 53 83)))))

(define transpose-tests
  (test-suite
   "Tests for transpose"

   (test-case "Transpose of 2x3 matrix"
     (check-equal? (transpose (list (list 1 2 3)
                                    (list 4 5 6)))
                   (list (list 1 4)
                         (list 2 5)
                         (list 3 6))))

   (test-case "Transpose of 3x3 square matrix"
     (check-equal? (transpose (list (list 1 2 3)
                                    (list 4 5 6)
                                    (list 7 8 9)))
                   (list (list 1 4 7)
                         (list 2 5 8)
                         (list 3 6 9))))

   (test-case "Transpose of single row"
     (check-equal? (transpose (list (list 1 2 3 4)))
                   (list (list 1) (list 2) (list 3) (list 4))))

   (test-case "Transpose of single column"
     (check-equal? (transpose (list (list 1) (list 2) (list 3)))
                   (list (list 1 2 3))))

   (test-case "Transpose of 1x1 matrix"
     (check-equal? (transpose (list (list 5)))
                   (list (list 5))))

   (test-case "Transpose of empty matrix"
     (check-equal? (transpose (list))
                   (list)))))

(define matrix-*-matrix-tests
  (test-suite
   "Tests for matrix-*-matrix"

   (test-case "Multiply 2x3 matrix by 3x2 matrix"
     (check-equal? (matrix-*-matrix (list (list 1 2 3)
                                          (list 4 5 6))
                                    (list (list 7 8)
                                          (list 9 10)
                                          (list 11 12)))
                   (list (list 58 64)
                         (list 139 154))))

   (test-case "Multiply 3x3 matrix by 3x3 matrix"
     (check-equal? (matrix-*-matrix (list (list 1 2 3)
                                          (list 4 5 6)
                                          (list 7 8 9))
                                    (list (list 9 8 7)
                                          (list 6 5 4)
                                          (list 3 2 1)))
                   (list (list 30 24 18)
                         (list 84 69 54)
                         (list 138 114 90))))

   (test-case "Multiply 2x2 identity matrix by 2x2 matrix"
     (check-equal? (matrix-*-matrix (list (list 1 0)
                                          (list 0 1))
                                    (list (list 5 6)
                                          (list 7 8)))
                   (list (list 5 6)
                         (list 7 8))))

   (test-case "Multiply 3x1 matrix by 1x3 matrix"
     (check-equal? (matrix-*-matrix (list (list 2)
                                          (list 3)
                                          (list 4))
                                    (list (list 1 2 3)))
                   (list (list 2 4 6)
                         (list 3 6 9)
                         (list 4 8 12))))

   (test-case "Multiply 1x3 matrix by 3x1 matrix"
     (check-equal? (matrix-*-matrix (list (list 1 2 3))
                                    (list (list 4)
                                          (list 5)
                                          (list 6)))
                   (list (list 32))))

   (test-case "Multiply 1x1 matrix by 1x1 matrix"
     (check-equal? (matrix-*-matrix (list (list 5))
                                    (list (list 7)))
                   (list (list 35))))))

(run-tests dot-product-tests)
(run-tests matrix-*-vector-tests)
(run-tests transpose-tests)
(run-tests matrix-*-matrix-tests)
