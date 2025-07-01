#lang racket

(
  define tolerance 0.00001
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
          let ( ( next ( f guess ) ) )
          (
            if ( close-enough? guess next )
              next
              ( try next )
          )
        )
    )

    ( try first-guess )
)

(
  define ( average_of_2 a b )
    ( / ( + a b ) 2 )
)

; SQRT - PLAIN : DOES NOT CONVERGE
#|
(
  define ( sqrt x )
    ( fixed-point ( lambda ( y ) ( / x y ) ) 1.0 )
)
|#

; SQRT - AVG. DUMPING
(
  define ( sqrt x )
    ( fixed-point ( lambda ( y ) ( average_of_2 y ( / x y ) ) ) 1.0 )
)

(
  define ( cbrt x )
    ( fixed-point ( lambda ( y ) ( average_of_2 y ( / x ( * y y ) ) ) ) 1.0 )
)

; FTRT - AVG. DUMPING : DOES NOT CONVERGE
#|
(
  define ( ftrt x )
    ( fixed-point ( lambda ( y ) ( average_of_2 y ( / x ( * y y y ) ) ) ) 1.0 )
)
|#

(
  define ( compose f g )
    ( lambda ( x ) ( f ( g x ) ) )
)

(
  define ( repeated f n )
    (
      if ( = n 1 )
        f
        ( compose f ( repeated f ( - n 1 ) ) )
    )
)
(
  define ( average-dump f )
    ( lambda ( x )
      ( average_of_2 x ( f x ) ) )
)
(
 define ( ftrt x )
)

