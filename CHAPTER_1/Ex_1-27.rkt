#lang racket

(
  define ( square x )
    ( * x x )
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
  define ( is-congruent? n a )
    ( if ( = ( expmod a n n ) a )
         true
         false
    )
)

(
  define ( fermat-test-inner n a )
    ( cond ( ( = a 0 ) true )
           ( ( is-congruent? n a ) ( fermat-test-inner n ( - a 1 ) ) )
           ( else false )
    )
)

(
  define ( fermat-test n )
    ( fermat-test-inner n ( - n 1 ) )
)

( fermat-test 561 )
( fermat-test 1105 )
( fermat-test 1729 )
( fermat-test 2465 )
( fermat-test 2821 )
( fermat-test 6601 )

