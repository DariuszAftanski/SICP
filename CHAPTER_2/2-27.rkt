#lang racket

; Exercise 2.18

#|
(define (reverse a_list)
  (if (null? (cdr a_list))
      (append a_list)
      (append (reverse (cdr a_list)) (list (car a_list))))
)
|#

(define (reverse a_list)
  (if (null? a_list)
      null
      (append (reverse (cdr a_list)) (list (car a_list))))
)

(define (deep-reverse a_list)
  (cond
    ((null? a_list) null)
    ((pair? a_list) (append (deep-reverse (cdr a_list)) (list (deep-reverse (car a_list)))))
    (else a_list)
  )
)

(define x (list (list 1 2) (list 3 4)))
x
(deep-reverse x)
(newline)
