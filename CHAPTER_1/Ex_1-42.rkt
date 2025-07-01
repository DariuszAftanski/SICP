#lang racket

(
  define ( inc n )
    ( + n 1 )
)

(
  define ( square x )
    ( * x x )
)

(
  define ( compose f g )
    ( lambda ( x ) ( f ( g x ) ) )
)

; USE CASES:
( display "((compose square inc) 6)" )
( newline )
( display ( ( compose square inc ) 6 ) )
