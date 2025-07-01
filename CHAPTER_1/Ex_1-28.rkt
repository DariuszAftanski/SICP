#lang racket

(
  define ( square x )
    ( * x x )
)

(
  define ( remainder-on-square-operation-with-check value m )
    ( if ( and ( not ( or ( = value 1)
                          ( = value ( - m 1) ) ) )
               ( = ( square value ) 1 ) )
         0
         ( remainder ( square value ) m )
    )
)

(
  define ( expmod base exp m )
    ( cond ( ( = exp 0 )
            1 )
           ( ( even? exp )
            ( remainder-on-square-operation-with-check ( expmod base ( / exp 2 ) m ) m ) )
           ( else
            ( remainder ( * base ( expmod base ( - exp 1 ) m ) ) m ) )
    )
)

(
  define ( is-congruent? n a )
    ( if ( = ( expmod a ( - n 1 ) n ) 1 )
         true
         false
    )
)

(
  define ( mr-test-inner n a )
    ( cond ( ( = a 0 ) true )
           ( ( is-congruent? n a ) ( mr-test-inner n ( - a 1 ) ) )
           ( else false )
    )
)

(
  define ( mr-test n )
    ( mr-test-inner n ( - n 1 ) )
)

( mr-test 561 )
( mr-test 1105 )
( mr-test 1729 )
( mr-test 2465 )
( mr-test 2821 )
( mr-test 6601 )

