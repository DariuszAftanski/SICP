#lang racket

(
  define ( abs x )
    (
      if ( < x 0 )
        ( - x )
        x 
    )
)

(
  define ( square x )
    ( * x x )
)

(
  define ( cube x )
    ( * x x x)
)

(
  define ( average x y )
    ( / ( + x y ) 2 )
)

(
  define ( good-enough? guess next-guess )
    ( < ( abs ( / ( -  guess  next-guess ) guess ) ) 0.000001 )
)

(
  define ( improve guess radicand )
    ( / ( + ( / radicand (square guess)) (* 2 guess)) 3)
)

(
  define ( display-radicand-and-guess radicand guess )
    (display radicand)
    (display " : ")
    (display guess)
    (display "\n") 
)

(
  define ( cbrt-iter guess radicand )
    ( display-radicand-and-guess radicand guess )
    
    (
      if ( good-enough? guess ( improve guess radicand ) )
        guess
        ( cbrt-iter ( improve guess radicand ) radicand )
    )
)
