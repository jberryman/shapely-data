shapely-data is a haskell library [up here on hackage](http://hackage.haskell.org/package/shapely-data)
that uses template haskell to convert to and from normal algebraic data types
and a representation using only haskell's primitive product, sum and unit
types: `(,)`, `Either`, and `()`.

See library docs for usage. Install it with a 

    cabal install shapely-data

This is very much a messy proof-of-concept library at the moment. Let me know
id you find it useful or have an idea for additional functionality/directions.
