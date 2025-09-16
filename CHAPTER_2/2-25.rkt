#lang racket

(require "Helper.rkt")

(define list_1 (list 1 3 (list 5 7) 9))

(display list_1)
(newline)
(car (cdr (car (cdr (cdr list_1)))))
(newline)

(define list_2 (list (list 7)))
(display list_2)
(newline)
(car (car list_2))
(newline)

(define list_3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display list_3)
(newline)
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list_3))))))))))))
(newline)
