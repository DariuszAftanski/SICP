#lang racket

(require "Helper.rkt")

; 2.1.4 Extended Exercise: Interval Arithmetic

#|
Alyssa’s idea is to implement “interval arithmetic” as a set of arithmetic
operations for combining “intervals” (objects that represent the range of
possible values of an inexact quantity). The result of adding, subtracting,
multiplying, or dividing two intervals is itself an interval, representing the
range of the result.
|#
#|
Alyssa postulates the existence of an abstract object called an “interval” that
has two endpoints: a lower bound and an upper bound. She also presumes that,
given the endpoints of an interval, she can construct the interval using the
data constructor make-interval. Alyssa ﬁrst writes a procedure for adding two
intervals. She reasons that the minimum value the sum could be is the sum of the
two lower bounds and the maximum value it could be is the sum of the two upper
bounds:
|#

(define (add-interval interval-x interval-y)
  (make-interval (+ (lower-bound interval-x) (lower-bound interval-y))
                 (+ (upper-bound interval-x) (upper-bound interval-y))
  )
)

#|
Alyssa also works out the product of two intervals by ﬁnding the minimum and the
maximum of the products of the bounds and using them as the bounds of the
resulting interval. (min and max are primitives that find the minimum or maximum
of any number of arguments.)
|#

(define (mul-interval interval-x interval-y)
  (let ((p1 (* (lower-bound interval-x) (lower-bound interval-y)))
        (p2 (* (lower-bound interval-x) (upper-bound interval-y)))
        (p3 (* (upper-bound interval-x) (lower-bound interval-y)))
        (p4 (* (upper-bound interval-x) (upper-bound interval-y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4)))
)

#|
To divide two intervals, Alyssa multiplies the first by the reciprocal of the
second. Note that the bounds of the reciprocal interval are the reciprocal of
the upper bound and the reciprocal of the lower bound, in that order.
|#

(define (div-interval interval-x interval-y)
  (mul-interval
    interval-x
    (make-interval (/ 1.0 (upper-bound interval-y))
                   (/ 1.0 (lower-bound interval-y))))
)

; Exercise 2.7
(display "Exercise 2.7:") (newline)

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))

(define test_1 (make-interval 2 3))
(display "(2 3) should be (2 3)") (newline)
(lower-bound test_1)
(upper-bound test_1)
(newline)

(display "(6 2) should be (2 6)") (newline)
(define test_2 (make-interval 6 2))
(lower-bound test_2)
(upper-bound test_2)
(newline)

; Exercise 2.8
(display "Exercise 2.8:") (newline)

(define (sub-interval interval-x interval-y)
  (make-interval (- (lower-bound interval-x) (upper-bound interval-y))
                 (- (upper-bound interval-x) (lower-bound interval-y)))
)

(display "(6 2) - (2 3) should be (-1 4)") (newline)
(define test_3 (sub-interval test_2 test_1))
(lower-bound test_3)
(upper-bound test_3)
(newline)

; Exercise 2.9
(display "Exercise 2.9:") (newline)

(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0)
)

(display "Difference of two intervals:") (newline)
(display "(6 2)(w:2) - (2 3)(w:0.5) should be (-1 4)(w:2.5)") (newline)
(width-interval test_2)
(width-interval test_1)
(width-interval test_3)
(newline)

(display "Sum of two intervals:") (newline)
(display "(6 2)(w:2) + (2 3)(w:0.5) should be (4 9)(w:2.5)") (newline)
(define test_4 (add-interval test_2 test_1))
(width-interval test_2)
(width-interval test_1)
(width-interval test_4)
(newline)

(display "Multiplication of two intervals:") (newline)
(display "(6 2)(w:2) + (2 3)(w:0.5) should be (4 18)(w:7)") (newline)
(define test_5 (mul-interval test_2 test_1))
(width-interval test_2)
(width-interval test_1)
(width-interval test_5)
(newline)

; Exercise 2.10
(display "Exercise 2.10:") (newline)

(define (div-interval-safe interval-x interval-y)
  (define (spans-zero? interval)
    (negative? (* (lower-bound interval) (upper-bound interval)))
  )

  (if (spans-zero? interval-y)
      (display "Error: interval spans zero.")
      (mul-interval
        interval-x
        (make-interval (/ 1.0 (upper-bound interval-y))
                       (/ 1.0 (lower-bound interval-y)))))
)

(define test_6 (make-interval (- 1) 2))

(display "Division of two intervals:") (newline)
(display "(6 2) / (- 1 2) should be (-6 3)") (newline)
(div-interval test_2 test_6)
(newline)

(display "Safe division of two intervals:") (newline)
(display "(6 2) / (- 1 2) should be ERROR") (newline)
(div-interval-safe test_2 test_6)
(newline)

; Exercise 2.11
(display "Exercise 2.11:") (newline)

#|
All combinations:
(mul (+|+) (+|+))
(mul (+|+) (+|-))
(mul (+|+) (-|+))
(mul (+|+) (-|-))
(mul (+|-) (+|+))
(mul (+|-) (+|-))
(mul (+|-) (-|+))
(mul (+|-) (-|-))
(mul (-|+) (+|+))
(mul (-|+) (+|-))
(mul (-|+) (-|+))
(mul (-|+) (-|-))
(mul (-|-) (+|+))
(mul (-|-) (+|-))
(mul (-|-) (-|+))
(mul (-|-) (-|-))

Now, let's simplify by assuming that LHS value cannot be positive while
RHS value is negative.
(mul (+|+) (+|+))
(mul (+|+) (-|+))
(mul (+|+) (-|-))
(mul (-|+) (+|+))
(mul (-|+) (-|+)) ----
(mul (-|+) (-|-))
(mul (-|-) (+|+))
(mul (-|-) (-|+))
(mul (-|-) (-|-))

In this case knowing the sign of expressions does not help, as two negative
lower-bounds multiplied can be greater that two positive upper-bounds.
|#

#|
After debugging her program, Alyssa shows it to a potential user, who complains
that her program solves the wrong problem. He wants a program that can deal with
numbers represented as a center value and an additive tolerance; for example, he
wants to work with intervals such as 3.5 ± 0.15 rather than [3.35, 3.65].
Alyssa returns to her desk and fixes this problem by supplying an alternate
constructor and alternate selectors:
|#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Exercise 2.12
(display "Exercise 2.12:") (newline)

(define (make-center-percent c tolerance-percentage)
  (make-center-width c (* c tolerance-percentage 0.01))
)

(define (_percent i)
  (* (/ (width i) (center i)) 100)
)

(define (test-ex_2-12)
  (define value 110)
  (define percent 1)
  (define interval (make-center-percent value percent))
  (display interval) (newline)
  (display (lower-bound interval)) (newline)
  (display (upper-bound interval)) (newline)
  (display (center interval)) (newline)
  (display (width interval)) (newline)
  (display (_percent interval)) (newline)

)

; (test-ex_2-12)

; Exercise 2.13
(display "Exercise 2.13:") (newline)

; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

(define R_1 (make-center-percent 10000 1))
(define R_2 (make-center-percent 100 1))

(par1 R_1 R_2)
(par2 R_1 R_2)

(div-interval-safe R_1 R_1)
(div-interval-safe R_2 R_2)

(mul-interval R_1 R_1)
(mul-interval R_2 R_2)

; The more "variables" are used the more tolerance error they produce.














