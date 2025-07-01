#lang racket

(
  define ( compose f g )
    ( lambda ( x ) ( f ( g x ) ) )
)

(
  define ( repeated f n )
    (
      if ( = n 2 )
        f
        ( compose f ( repeated f ( - n 1 ) ) )
    )
)

( define dx 0.0001 )

(
  define ( smooth f )
    ( lambda ( x )
      ( /
          ( + ( f ( - x dx ) )
              ( f x )
              ( f ( + x dx ) )
          )
          3
      )
    )
)

(
  define ( smooth-n-fold f n )
    ( ( repeated smooth n ) f )
)

(
  define ( square x )
    ( * x x )
)
