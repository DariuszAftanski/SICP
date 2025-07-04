#lang racket

(
  define ( square x )
    ( * x x )
)

(
  define ( divides? a b )
    ( = ( remainder b a ) 0 )
)

(
  define ( find-divisor n test-divisor )
    ( display "TESTED DIVISOR: " ) ( display test-divisor ) ( newline )

    ( cond ( ( > ( square test-divisor ) n ) n )
           ( ( divides? test-divisor n ) test-divisor )
           ( else (find-divisor n ( + test-divisor 1 ) ) )
    )
)

(
  define ( smallest-divisor n )
  ( find-divisor n 2 )
)

; TEST CASES:
( smallest-divisor 199 )
( smallest-divisor 1999 )
( smallest-divisor 19999 )
