#lang racket

(provide average)
(provide square)

(define (average a b)
  (/ (+ a b) 2.0)
)

(define (square a)
  (* a a)
)

#|
(define (count-leaves x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x)))
    )
  )
)
|#

