#lang racket

; Exercise 2.18

(define (fringe a_list)
  (cond
    ((null? a_list) null)
    ((pair? a_list) (append (fringe (car a_list)) (fringe (cdr a_list))))
    (else (list a_list))
  )
)

(define x (list (list 1 2 3) (list 4 5 6)))
x
(fringe x)
(newline)

(define y (list x x))
y
(fringe y)
(newline)
