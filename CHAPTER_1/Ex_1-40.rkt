#lang racket

(
  define tolerance 0.000000001
)

(
  define ( fixed-point f first-guess )

    (
      define ( close-enough? v1 v2 )
        ( < (abs (- v1 v2) ) tolerance )
    )

    (
      define ( try guess )
        ( display guess ) ( newline )

        (
          let ( ( next (f guess ) ) )
          (
            if ( close-enough? guess next )
              next
              ( try next )
          )
        )
    )

    ( try first-guess )
)

( define dx 0.000001 )

(
  define ( deriv g )
    ( lambda ( x ) ( / ( - ( g ( + x dx ) ) ( g x ) ) dx ) )
)

(
  define ( newton-transform g )
    ( lambda ( x ) ( - x ( / ( g x ) ( ( deriv g ) x ) ) ) )
)

(
  define ( newtons-method g guess )
    ( fixed-point ( newton-transform g ) guess )
)

(
  define ( cubic x )
    ( * x x x)
)

(
  define ( square x )
    ( * x x )
)

(
  define ( cubic-polynomial a b c )
    (lambda ( x ) ( + ( cubic x ) ( * a ( square x ) ) ( * b x )  c  ) )
)

; TEST CASES

( display (newtons-method (cubic-polynomial -6 12 -8 ) 4 ) )
