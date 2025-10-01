#lang racket

(require "Helper.rkt")

#|
Exercise 2.29

A binary mobile consists of two branches, a left branch and a right branch. Each
branch is a rod of a certain length, from which hangs either a weight or another
binary mobile. We can represent a binary mobile using compound data by
constructing it from two branches (for example, using list):
|#

(
  define (make-mobile left right)
    (list left right)
)

#|
A branch is constructed from a length (which must be a number) together with
a structure, which may be either a number (representing a simple weight) or
another mobile:
|#

(
  define (make-branch length structure)
    (list length structure)
)

#|
a.:
Write the corresponding selectors left-branch and right-branch, which return
the branches of a mobile, and branch-length and branch-structure, which return
the components of a branch.
|#

(
  define (left-branch mobile)
    (car mobile)
)

(
  define (right-branch mobile)
    (car (cdr mobile))
)

(
  define (branch-length branch)
    (car branch)
)

(
  define (branch-structure branch)
    (car (cdr branch))
)

#|
b.:
Using your selectors, define a procedure total-weight that returns the total
weight of a mobile.
|#

(
  define (mobile? structure)
    (pair? structure)
)

(
  define (branch-weight branch)
    (let ([structure (branch-structure branch)])
      (if (mobile? structure)
          (total-weight structure)
          structure
      )
    )
)

(
  define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile)))
)

; Test:
(
  define (test-b)
    (define mobile-1 (make-mobile (make-branch 1 2) (make-branch 3 4)))
    (define mobile-2 (make-mobile (make-branch 1 2) (make-branch 3 mobile-1)))

    (display "mobile-1 = ")
    (display mobile-1)
    (newline)
    (display "(total-weight mobile-1) = ")
    (display (total-weight mobile-1))  ; should be 6
    (newline)

    (newline)

    (display "mobile-2 = ")
    (display mobile-2)
    (newline)
    (display "(total-weight mobile-2) = ")
    (display (total-weight mobile-2))  ; should be 8
    (newline)

    (newline)
)

#|
c.:
A mobile is said to be balanced if the torque applied by its top-left branch is
equal to that applied by its top-right branch (that is, if the length of the
left rod multiplied by the weight hanging from that rod is equal to the
corresponding product for the right side) and if each of the submobiles hanging
off its branches is balanced. Design a predicate that tests whether a binary
mobile is balanced.
|#

(
  define (torque branch)
    (* (branch-length branch)
       (branch-weight branch))
)

(
  define (balanced? mobile)
    (let ([lbranch (left-branch mobile)]
          [rbranch (right-branch mobile)]
          [lbranch-structure (branch-structure (left-branch mobile))]
          [rbranch-structure (branch-structure (right-branch mobile))])
      (and (= (torque lbranch) (torque rbranch))
           (or (not (mobile? lbranch-structure)) (balanced? lbranch-structure))
           (or (not (mobile? rbranch-structure)) (balanced? rbranch-structure)))
    )
)

; Test:
(
  define (test-c)
    (define mobile-1 (make-mobile (make-branch 1 2) (make-branch 2 1)))
    (define mobile-2 (make-mobile (make-branch 1 2) (make-branch 3 mobile-1)))
    (define mobile-3 (make-mobile (make-branch 3 4) (make-branch 6 2)))
    (define mobile-4 (make-mobile (make-branch 3 4) (make-branch 4 mobile-1)))

    (display "mobile-1 = ")
    (display mobile-1)
    (newline)
    (display "(balanced? mobile-1) = ")
    (display (balanced? mobile-1))
    (newline)

    (newline)

    (display "mobile-2 = ")
    (display mobile-2)
    (newline)
    (display "(balanced? mobile-2) = ") ; should not be balanced.
    (display (balanced? mobile-2))
    (newline)

    (newline)

    (display "mobile-3 = ")
    (display mobile-3)
    (newline)
    (display "(balanced? mobile-3) = ")
    (display (balanced? mobile-3))
    (newline)

    (newline)

    (display "mobile-4 = ")
    (display mobile-4)
    (newline)
    (display "(balanced? mobile-4) = ")
    (display (balanced? mobile-4))
    (newline)

    (newline)
)

#|
d.:
Suppose we change the representation of mobiles so that the constructors are:
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
How much do you need to change your programs to
convert to the new representation?
|#

#|
ANS:  When setters are changed, only getters need to be changed to reflect the
      changes. Actually, this implementation should come to mind in the first
      place in my opinion.
|#

