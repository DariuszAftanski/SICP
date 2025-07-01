#lang racket

(
  define ( display_gcd_entry a b )
    (display "GCD: ")
    (display a)
    (display ", ")
    (display b)
    (display "\n")
)

(
  define ( gcd a b )
    ( display_gcd_entry a b )

    ( if ( = b 0 )
        a
        ( gcd b ( remainder a b ) )
    )
)

(
  define ( gcd_counter a b steps_left )
    ( display "Executed " ) ( display b ) ( display "\n" )

    ( if ( = steps_left 1 )
        (begin (display "Executed ") (display a) (display "\n"))
        ( gcd_counter b ( + a b 1 ) ( - steps_left 1 ) )
    )
)


#|
  1. Applicative order: 4

  2. Normal order:

  ( gcd 206 40 ):
    ( if ( = 40 0 )
        ...
        ( gcd 40 ( remainder 206 40 ) )
    )

  ( gcd 40 ( remainder 206 40 )):
    ( if ( = ( remainder 206 40 ) 0 ) : EXECUTED 1 * remainder
        ...
        ( gcd ( remainder 206 40 ) ( remainder 40 ( remainder 206 40 ) ) )
    )

  ( gcd ( remainder 206 40 ) ( remainder 40 ( remainder 206 40 ) ) ):
    ( if ( = ( remainder 40 ( remainder 206 40 ) ) 0 ) ) : Executed 2 * remainder
      ...
      (gcd ( remainder 40 ( remainder 206 40 ) ) ( remainder (remainder 206 40) ( remainder 40 ( remainder 206 40 ) ) ) )

  This could be represented as:
  -> GCD C C [ aka GCD: 206, 40 ]:
  ----> Executed none.
  ----> GCD: C 1N

  -> GCD: C 1N [ aka GCD: 40 6]
  ----> Executed: 1
  ----> GCD: 1N C+1N+1N

  -> GCD: 1N 2N [ aka GCD: 6 4 ]
  ----> Executed: 2
  ----> GCD: 2N 1N+2N+1N

  -> GCD: 2N 4N [ aka GCD: 4 2]
  ----> Executed: 4
  ----> GCD: 4N 2N+4N+1N

  -> GCD: 4N 7N [ aka GCD: 2 0]
  ----> Executed: 7
  ----> Executed: 4

  C should was here to represent constant values that are not using remainder operation,
  but in this arithmetic, it could be understood as 0.
  ( GCD_NEXT b a+b+1 ) when ( GCD a b )

|#
