#lang racket

; Exercise 2.17

(define (last-pair a_list)
  (if (null? (cdr a_list))
      (list (car a_list))
      (last-pair (cdr a_list)))
)
