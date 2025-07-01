#lang racket

(
  define ( sum term a next b )
    (
      if ( > a b )
        0
        ( +
            ( term a )
            ( sum term ( next a ) next b )
        )
    )
)

(
  define ( cube n )
    ( * n n n )
)

;---------------------------------------------------------------------

(
  define ( simpsons-rule-approx f a b n )
    ; Internal functions :
    (
      define h
        ( / ( - b a ) n )
    )
    (
      define ( inc_by_2h n )
        ( + n h h )
    )

    ; Function body :
    ( *
        ( +
            ( f a )
            ( f b )
            ( * 4.0 ( sum f ( + a h ) inc_by_2h  b ) )
            ( * 2.0 ( sum f ( + a h h ) inc_by_2h ( - b h ) ) )
        )
        ( / h 3.0 )
    )
)

































