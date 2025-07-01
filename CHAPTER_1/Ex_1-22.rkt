#lang racket

; Racket primitive: current-inexact-milliseconds
; Returns an integer that specifies the amount of time the system has been
; running in milliseconds.

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
;   ( display "TESTED DIVISOR: " ) ( display test-divisor ) ( newline )

    ( cond ( ( > ( square test-divisor ) n ) n )
           ( ( divides? test-divisor n ) test-divisor )
           ( else (find-divisor n ( + test-divisor 1 ) ) )
    )
)

(
  define ( smallest-divisor n )
  ( find-divisor n 2 )
)

(
 define ( prime? n )
  ( = n ( smallest-divisor n ) )
)

(
  define ( report-prime elapsed-time )
    ( display "  *** ")
    ( display elapsed-time )
)

(
  define ( start-prime-test n start-time )
    ( if ( prime? n )
      ( report-prime ( - ( current-inexact-milliseconds ) start-time ) )
      ( display "" )
    )
)
(
  define ( timed-prime-test n )
    ( newline )
    ( display n )
    ( start-prime-test n ( current-inexact-milliseconds ) )
)

(
  define ( search-for-primes n upper-boundary )
    ( if ( divides? 2 n )
         ( inner-search-for-primes ( + n 1 ) upper-boundary )
         ( inner-search-for-primes n upper-boundary )
    )
)

(
  define (inner-search-for-primes n upper-boundary)
    ( if ( < n upper-boundary )
         ( begin
           ( timed-prime-test n )
           ( inner-search-for-primes (+ n 2) upper-boundary )
         )
         ( newline )
    )
)
; TEST CASES:
( search-for-primes 1000 1020 )
( search-for-primes 10000 10040 )
( search-for-primes 100000 100050 )
( search-for-primes 1000000 1000050 )
( search-for-primes 10000000 10000050 )
( search-for-primes 100000000 100000050 )

