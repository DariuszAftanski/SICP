#lang racket

(
  define ( f n )
  (
    if ( < n 3 )
      n
      ( + ( f ( - n 1) )
          ( * 2 ( f ( - n 2) ) )
          ( * 3 ( f ( - n 3) ) ) )
  )
)

(
  define (f-internal n_0 n_1 n_2 n_to_go)
    (
      if ( = n_to_go 0 )
        n_2
        ( f-internal n_1 n_2 ( + n_2 ( * 2 n_1 ) ( * 3 n_0 ) ) ( - n_to_go 1 ) )
    )
)

(
  define (f-iter n )
  (
    if ( < n 3 )
      n
      (f-internal 0 1 2 ( - n 2 ) )
  )
)
