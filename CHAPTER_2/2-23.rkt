#lang racket

(require "Helper.rkt")

#|
(
  define (for-each-2 procedure items)
    (cond ((null? items) (void))
          (else
            (define _item (car items))
            (define _items (cdr items))
            (procedure _item)
            (for-each-2 procedure _items)
          )
    )
)
|#

(
  define (for-each-2 procedure items)
    (if (null? items)
        (void)
        (let ((_item (car items))
              (_items (cdr items)))
          (procedure _item)
          (for-each-2 procedure _items))
    )
)

; TEST CASES
; for-each
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
; for-each-2
(for-each-2 (lambda (x) (newline) (display x)) (list 57 321 88))
