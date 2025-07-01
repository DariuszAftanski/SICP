#lang racket

(
  define ( abs x )
    ( if ( < x 0 ) ( - x ) x )
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
  define ( good-enough? guess radicand )
    ( < ( abs ( - ( square guess ) radicand ) ) 0.001 )
)

(
  define ( new-good-enough? guess next-guess )
    ( < ( abs ( / ( -  guess  next-guess ) guess ) ) 0.001 )
)

(
  define ( improve guess radicand )
    ( average guess ( / radicand guess) )
)

(
  define ( display-radicand-and-guess radicand guess )
    (display radicand)
    (display " : ")
    (display guess)
    (display "\n") 
)

(
  define ( sqrt-iter guess radicand )
    ( display-radicand-and-guess radicand guess )
    
    (
      if ( good-enough? guess radicand )
        guess
        ( sqrt-iter ( improve guess radicand ) radicand )
    )
)

(
  define ( new-sqrt-iter guess radicand )
    ( display-radicand-and-guess radicand guess )
    
    (
      if ( new-good-enough? guess ( improve guess radicand ) )
        guess
        ( new-sqrt-iter ( improve guess radicand ) radicand )
    )
)
