#lang racket

; Exercise 2.18

(define (reverse a_list)
  (if (null? (cdr a_list))
      (append a_list)
      (append (reverse (cdr a_list)) (list (car a_list))))
)
