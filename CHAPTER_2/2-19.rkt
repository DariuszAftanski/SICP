#lang racket

(require "2-18.rkt")

; Exercise 2.19

(define (count-change-original amount) (cc-original amount 5))

(define (cc-original amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-original amount (- kinds-of-coins 1))
                 (cc-original (- amount (first-denomination-original kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination-original kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount) (cc amount us-coins))

(define (cc amount coin-values)
  (define (no-more? _coin-values) (null? _coin-values))
  (define (except-first-denomination _coin-values) (cdr _coin-values))
  (define (first-denomination _coin-values) (car _coin-values))

  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define test-value 21)
(display "test-value = ")
(display test-value)
(newline)

(display "(count-change test-value) = ")
(count-change test-value)

(display "(count-change-original test-value) = ")
(count-change-original test-value)

(display "(cc test-value us-coins) = ")
(cc test-value us-coins)

(display "(cc test-value (reverse us-coins)) = ")
(cc test-value (reverse us-coins))
