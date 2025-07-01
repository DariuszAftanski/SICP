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
  define ( expmod base exp m )
    ( cond ( ( = exp 0 )
            1 )
           ( ( even? exp )
            ( remainder ( square ( expmod base ( / exp 2 ) m ) ) m ) )
           ( else
             ( remainder ( * base ( expmod base ( - exp 1 ) m ) ) m ) )
    )
)

(
  define ( fermat-test n )
    ( define ( try-it a )
      ( = ( expmod a n n ) a ) )

    ( try-it ( + 1 ( random ( - n 1 ) ) ) )
)

(
  define ( fast-prime? n times )
    ( cond ( ( = times 0 ) true )
           ( ( fermat-test n ) ( fast-prime? n ( - times 1 ) ) )
           ( else false )
    )
)

(
  define ( report-prime elapsed-time )
    ( display "  *** ")
    ( display elapsed-time )
)

(
  define ( start-prime-test n start-time )
    ( if ( fast-prime? n 12 )
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
  define (inner-search-for-primes n upper-boundary)
    ( if ( < n upper-boundary )
         ( begin
           ( timed-prime-test n )
           ( inner-search-for-primes (+ n 2) upper-boundary )
         )
         ( newline )
    )
)

(
  define ( search-for-primes n upper-boundary )
    ( if ( divides? 2 n )
         ( inner-search-for-primes ( + n 1 ) upper-boundary )
         ( inner-search-for-primes n upper-boundary )
    )
)

; TEST CASES:
( search-for-primes 1000 1020 )
( search-for-primes 10000 10040 )
( search-for-primes 100000 100050 )
( search-for-primes 1000000 1000050 )
( search-for-primes 10000000 10000050 )
( search-for-primes 100000000 100000050 )

