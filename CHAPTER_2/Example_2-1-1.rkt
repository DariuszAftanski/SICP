#lang racket

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
(car x)
; 1
(cdr x)
; 2

#|
Notice that a pair is a data object that can be given a name and manipulated,
just like a primitive data object. Moreover, "cons" can be used to form pairs
whose elements are pairs, and so on:
|#
(set! x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
; 1
(car (cdr z))
; 3

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
our rational-number implementation does
not reduce rational numbers to lowest terms. We can remedy this by
changing make-rat. If we have a gcd procedure like the one in Section
1.2.5 that produces the greatest common divisor of two integers, we can
use gcd to reduce the numerator and the denominator to lowest terms
before constructing the pair:
|#

(define (make-rat n d)
  (define (extract-sign n d)
    (if (or (and (negative? n) (negative? d))
            (and (positive? n) (positive? d)))
        (+ 1)
        (- 1))
  )
  (let ((g (gcd n d))
        (sign (extract-sign n d)))
  (cons (/ ( * sign (abs n)) g) (/ (abs d) g)))
)

; multiply n by sign of d.














