#lang racket

(
  define ( iterative-improve good-enough? improve )
    (
      define ( iteration guess )
        (
          if ( good-enough? guess )
            guess
            ( iteration ( improve guess ) )
        )
    )
    ( lambda ( guess ) ( iteration guess) )
)

(
  define ( square x )
    ( * x x )
)

(
  define ( average x y )
    ( / ( + x y ) 2 )
)

(
  define ( sqrt x )
    (
      define ( good-enough? guess )
        ( < ( abs ( - ( square guess ) x ) ) 0.001 )
    )
    (
      define ( improve guess )
        ( average guess ( / x guess ) )
    )
    ( ( iterative-improve good-enough? improve ) 1.0 )
)

(
  define ( fixed-point f first-guess )
    ( define tolerance 0.00001 )
    (
      define ( improve guess )
        ( f guess )
    )
    (
      define ( good-enough? guess )
        ( < ( abs ( - guess ( improve guess ) ) ) tolerance )
    )

    ( ( iterative-improve good-enough? improve ) 1.0 )
)

(
  define ( sqrt_2 x )
    ( fixed-point ( lambda ( y ) ( average y ( / x y ) ) ) 1.0 )
)
