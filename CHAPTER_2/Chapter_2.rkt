#lang racket

(require "Helper.rkt")

; 2.1.1 Example: Arithmetic Operations for Rational Numbers

#|
Let us begin by assuming that we already have a way of constructing a rational
number from a numerator and a denominator.
We also assume that, given a rational number, we have a way of extracting (or
selecting) its numerator and its denominator.
Let us further assume that the constructor and selectors are available
as procedures:
-> ( make-rat ⟨ n ⟩⟨ d ⟩ )
    returns the rational number whose numerator is the integer ⟨ n ⟩ and whose
    denominator is the integer ⟨ d ⟩.
-> ( numer ⟨ x ⟩ )
    returns the numerator of the rational number ⟨ x ⟩.
-> ( denom ⟨ x ⟩ )
    returns the denominator of the rational number ⟨ x ⟩.

We could then add, subtract, multiply, divide, and test equality by
using the following relations:
|#

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))
  )
)

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))
  )
)

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))
  )
)

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))
  )
)

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x)))
)

#|
Now we have the operations on rational numbers defined in terms of the selector
and constructor procedures numer, denom, and make-rat.
But we haven’t yet defined these. What we need is some way to glue together
a numerator and a denominator to form a rational number.
|#

#|
Pairs
To enable us to implement the concrete level of our data abstraction, our
language provides a compound structure called a pair, which can be constructed
with the primitive procedure "cons". This procedure takes two arguments and
returns a compound data object that contains the two arguments as parts. Given
a pair, we can extract the parts using the primitive procedures "car" and "cdr".
Thus, we can use "cons", "car", and "cdr" as follows:
|#

(define x (cons 1 2))
;(car x)
; Display: 1
;(cdr x)
; Display: 2

#|
Notice that a pair is a data object that can be given a name and manipulated,
just like a primitive data object. Moreover, "cons" can be used to form pairs
whose elements are pairs, and so on:
|#
(set! x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
;(car (car z))
; Display: 1
;(car (cdr z))
; Display: 3

#|
Data obejcts constructed from pairs are called list-structured data.
|#

#|
Representing rational numbers
Pirs offer a natural way to complete the rational-number system. Simply
represent a rational number as a pair of two integers: a numerator and
a denominator.
Then make-rat, numer, and denom are readily implemented as follows:
|#

#|
(define (make-rat n d)
  (cons n d)
)
|#

(define (numer x)
  (car x)
)

(define (denom x)
  (cdr x)
)

#|
Also, in order to display the results of our computations, we can print
rational numbers by printing the numerator, a slash, and the denominator:
|#

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

#|
Our rational-number implementation does not reduce rational numbers to lowest
terms. We can remedy this by changing make-rat. If we have a gcd procedure
like the one in Section 1.2.5 that produces the greatest common divisor of
two integers, we can use gcd to reduce the numerator and the denominator to
lowest terms before constructing the pair:

(define (make-rat n d)
  (let ((g (gcd n d)))
  (cons (/ n g) (/ d g)))
)

|#

; Exercise 2.1

(define (make-rat n d)
  (define (extract-sign n d)
    (if (positive? d)
        (+ 1)
        (- 1))
  )
  (let ((g (gcd n d))
        (sign (extract-sign n d)))
  (cons (/ ( * sign n) g) (/ (abs d) g)))
)

(define (test-ex_2-1)
  (print-rat (make-rat 2 3))
  (print-rat (make-rat 2 (- 3)))
  (print-rat (make-rat (- 2) 3))
)

; Exercise 2.2

(define (make-segment p1 p2)
  (cons p1 p2)
)

(define (start-segment seg)
  (car seg)
)

(define (end-segment seg)
  (cdr seg)
)

(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p)
)

(define (y-point p)
  (cdr p)
)

(define (midpoint-segment seg)
  (let ((x1 (x-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y1 (y-point (start-segment seg)))
        (y2 (y-point (end-segment seg))))
  (make-point (average x1 x2) (average y1 y2)))
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define (test-ex_2-2)
  (define point_1 (make-point 0 0))
  (define point_2 (make-point 10 0))
  (define point_3 (make-point 0 10))
  (define point_4 (make-point 10 10))

  (print-point (midpoint-segment (make-segment point_1 point_4)))
  (print-point (midpoint-segment (make-segment point_1 point_2)))
  (print-point (midpoint-segment (make-segment point_1 point_3)))
)

; 2.1.3 What Is Meant by Data?

; Exercise 2.4
(define (test-ex_2-4)
  (define (cons x y)
    (lambda (m) (m x y))
  )
  (define (car z)
    (z (lambda (p q) p))
  )
  (define (cdr z)
    (z (lambda (p q) q))
  )

  (display "Testing exercise 2.4:")
  (newline)
  (display (car (cons 2 3)))
  (newline)
  (display (cdr (cons 2 3)))
  (newline)

  #|  Verify that (car (cons x y)) yields x for any objects x and y.
      (car (cons x y))
      (car (lambda (m) (m x y)))
      ((lambda (m) (m x y)) (lambda (p q) p))
      ----  First function takes as input a function that must be provided with
            two arguments. The first function provides the two required
            arguments, where the second functions handles them by returning
            the first argument.
  |#

  (define (mul z)
    (z (lambda (p q) (* p q)))
  )

  (display "Extra: multiplying:")
  (newline)
  (display (mul (cons 2 3)))
  (newline)
)

; Exercise 2.5
(define (test-ex_2-5)
  (define (cons x y)
    (* (expt 2 x) (expt 3 y))
  )
  (define (iterator combined_pair div count)
    (if (= 0 (remainder combined_pair div))
        (iterator (/ combined_pair div) div (+ count 1))
        count
    )
  )
  (define (car combined_pair)
    (iterator combined_pair 2 0)
  )
  (define (cdr combined_pair)
     (iterator combined_pair 3 0)
  )

  (display "(cons 2 3)       = ") (display (cons 2 3))
  (newline)
  (display "(car (cons 2 3)) = ") (display (car (cons 2 3)))
  (newline)
  (display "(cdr (cons 2 3)) = ") (display (cdr (cons 2 3)))
  (newline)
)

; Exercise 2.6
(define (test-ex_2-6)
  (define zero
    (lambda (f) (lambda (x) x)))
  (define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

  ; HINT, Use substitution to evaluate (add-1 zero)).
  ; (add-1 zero)
  ; (add-1 (lambda (f) (lambda (x) x))
  ;                              |
  ; (lambda (f) (lambda (x) (f ((n f) x))))
  ; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x) f) x))))
  ; HOW ?
  ; (lambda (f) (lambda (x) (f (f x))))
)



