#lang racket

; Problem 1

(
  define ( square x )
    ( * x x )
)

(
  define position ( lambda ( a v u t )
    ( + ( / ( * a ( square t ) ) 2.0 )
        ( * v t )
        u
    )
  )
)

; Problem 1: Test cases
; ( position 0 0 0 0 ) ; -> 0
; ( position 0 0 20 0 ) ; -> 20
; ( position 0 5 10 10 ) ; -> 60
; ( position 2 2 2 2 ) ; ->
; ( position 5 5 5 5 ) ; ->

; Problem 2:

(
  define ( delta a b c )
    ( - ( square b ) ( * 4 a c ) )
)

(
  define ( root1 a b c )
    ( if ( < ( delta a b c ) 0 )
         false
         ( / ( + ( - b ) ( sqrt ( delta a b c ) ) ) ( * 2 a ) )
    )
)

(
  define ( root2 a b c )
    ( if ( < ( delta a b c ) 0 )
         false
         ( / ( - ( - b ) ( sqrt ( delta a b c ) ) ) ( * 2 a ) )
    )
)

; Problem 2: Test cases:
;

; Problem 3

(
  define time-to-impact ( lambda ( vertical-velocity elevation )
    ( root2 ( / ( - 9.8 ) 2 ) vertical-velocity elevation )
  )
)

(
  define time-to-height( lambda ( vertical-velocity elevation target-elevation )
    ( root2 ( / ( - 9.8 ) 2 ) vertical-velocity ( - elevation target-elevation ) )
  )
)

; Problem 4

(
  define degree2radian ( lambda ( deg )
    ( / ( * deg pi ) 180. )
  )
)

(
  define travel-distance-simple ( lambda ( elevation velocity angle )
    ( * velocity
        ( cos ( degree2radian angle ) )
        ( time-to-impact ( * velocity ( sin ( degree2radian angle ) ) ) elevation )
    )
  )
)

; Problem 5

(
  define find-best-angle ( lambda ( velocity elevation )
    ( define initial-angle 1 )
    (
      define ( attempt velocity elevation angle )
        ( if ( > ( travel-distance-simple elevation velocity ( + angle 1 ) )  ( travel-distance-simple elevation velocity angle ) )
             ( attempt velocity elevation ( + angle 1 ) )
             angle
        )
    )
    ( attempt velocity elevation 1)
  )
)

; Problem 6

(define drag-coeff 0.5)
(define density 1.25)
(define mass .145)
(define diameter 0.074)
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))
(define gravity 9.8)

(
  define ( integrate x y u v dt g m beta )
  (
    if ( < y 0 )
      x
      ( integrate ( + x ( * u dt ) )
                  ( + y ( * v dt ) )
                  ( - u ( * ( / 1 m ) beta ( sqrt ( + ( square u ) ( square v ) ) ) u dt ) )
                  ( - v ( * ( + ( * ( / 1 m ) beta ( sqrt ( + ( square u ) ( square v ) ) ) v ) g ) dt ) ) dt g m beta )
  )
)

(
  define ( travel-distance elevation velocity angle )
    ( define alpha ( degree2radian angle ) )

    ( integrate 0 elevation ( * velocity ( cos alpha ) ) ( * velocity ( sin alpha ) ) 0.01 gravity mass beta)
)

; Project 7

(
  define ( integrate-time x y u v dt g m beta t )
  (
    if ( < y 0 )
      t
      ( integrate-time ( + x ( * u dt ) )
                  ( + y ( * v dt ) )
                  ( - u ( * ( / 1 m ) beta ( sqrt ( + ( square u ) ( square v ) ) ) u dt ) )
                  ( - v ( * ( + ( * ( / 1 m ) beta ( sqrt ( + ( square u ) ( square v ) ) ) v ) g ) dt ) ) dt g m beta ( + t dt ) )
  )
)

(
  define ( travel-time elevation velocity angle )
    ( define alpha ( degree2radian angle ) )

    ( integrate-time 0 elevation ( * velocity ( cos alpha ) ) ( * velocity ( sin alpha ) ) 0.01 gravity mass beta 0 )
)

; Project 8

(
  define ( travel-distance-with-bounce elevation velocity angle n )
    (
      define ( sum-of velocity angle n sum )
        ( display sum )
        ( newline )
        (
          if ( = n 0 )
            sum
            ( sum-of ( / velocity 2 ) angle ( - n 1 ) ( + sum ( travel-distance 0 ( / velocity 2) angle ) ) )
        )
    )
    ( sum-of  ( / velocity 2) angle n ( travel-distance elevation velocity angle ) )
)

; Project 9
(
  define ( integrate-with-bounce x y u v dt g m beta n )
    (
      if ( < y 0 )
        (
          if ( = n 0 )
            x
            ( integrate-with-bounce x 0 u (- v) dt g m beta ( - n 1 ) )
        )
        ( integrate-with-bounce  ( + x ( * u dt ) )
                                 ( + y ( * v dt ) )
                                 ( - u ( * ( / 1 m ) beta ( sqrt ( + ( square u ) ( square v ) ) ) u dt ) )
                                 ( - v ( * ( + ( * ( / 1 m ) ( sqrt ( + ( square u ) ( square v ) ) ) v beta ) g ) dt ) )
                                 dt g m beta n
        )
    )
)

(
  define ( travel-distance-bouncing-integrated elevation velocity angle n )
    (
      define alpha
        ( degree2radian angle )
    )
    (
      integrate-with-bounce 0 elevation ( * velocity ( cos alpha ) ) ( * velocity ( sin alpha ) ) 0.01 gravity mass beta n
    )
)






