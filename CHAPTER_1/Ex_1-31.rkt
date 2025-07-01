#lang racket

(
  define ( product term a next b )
    (
      if ( > a b )
        1
        ( *
            ( term a )
            ( product term ( next a ) next b )
        )
    )
)

(
  define ( inc n )
    ( + n 1 )
)

(
  define ( identity n )
    n
)

(
  define ( factorial n )
    ( product identity 1 inc n )
)
