#lang racket

(require "2-39.rkt")

(require rackunit)
(require rackunit/text-ui)

(define reverse_1-tests
  (test-suite
   "Tests for 'reverse_1' function"

   (test-case "Empty list"
     (check-equal? (reverse_1 (list)) (list)
                   "Reversing an empty list should return an empty list"))

   (test-case "Single element"
     (check-equal? (reverse_1 (list 1)) (list 1)
                   "Reversing a single-element list should return the same list"))

   (test-case "Multiple elements"
     (check-equal? (reverse_1 (list 1 2 3 4 5)) (list 5 4 3 2 1)
                   "Should reverse a list correctly"))))

(define reverse_2-tests
  (test-suite
   "Tests for 'reverse_2' function"

   (test-case "Empty list"
     (check-equal? (reverse_2 (list)) (list)
                   "Reversing an empty list should return an empty list"))

   (test-case "Single element"
     (check-equal? (reverse_2 (list 1)) (list 1)
                   "Reversing a single-element list should return the same list"))

   (test-case "Multiple elements"
     (check-equal? (reverse_2 (list 1 2 3 4 5)) (list 5 4 3 2 1)
                   "Should reverse a list correctly"))))


(run-tests reverse_1-tests)
(run-tests reverse_2-tests)
