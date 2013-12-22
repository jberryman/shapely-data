`shapely-data` is a haskell library [up here on hackage](http://hackage.haskell.org/package/shapely-data)
for working with algebraic datatypes in a simple generic form made up of
haskell's primitive product, sum and unit types: `(,)`, `Either`, and `()`.

You can install it with

    cabal install shapely-data

# Motivation and examples

In order from most to least important to me, here are the concerns that
motivated the library:

- Provide a good story for `(,)`/`Either` as a /lingua franca/ generic
  representation that other library writers can use without dependencies,
  encouraging abstractions in terms of products and sums (motivated
  specifically by my work on
  [`simple-actors`](http://hackage.haskell.org/package/simple-actors).

- Support algebraic operations on ADTs, making types composable

    -- multiplication:
    let a = (X,(X,(X,())))
        b = Left (Y,(Y,())) :: Either (Y,(Y,())) (Z,())
        ab = a >*< b
     in ab == ( Left (X,(X,(X,(Y,(Y,()))))) 
                :: Either (X,(X,(X,(Y,(Y,()))))) (X,(X,(X,(Z,())))) )

    -- exponentiation:
    fanout (head,(tail,(Prelude.length,()))) [1..3]    == (1,([2,3],(3,())))
    (unfanin (_4 `ary` (shiftl . Sh.reverse)) 1 2 3 4) == (3,(2,(1,(4,()))))

- Support powerful, typed conversions between `Shapely` types

    data F1 = F1 (Maybe F1) (Maybe [Int]) deriving Eq
    data F2 = F2 (Maybe F2) (Maybe [Int]) deriving Eq
    f2 :: F2
    f2 = coerce (F1 Nothing $ Just [1..3])

    data Tsil a = Snoc (Tsil a) a | Lin deriving Eq
    truth = massage "123" == Snoc (Snoc (Snoc Lin '3') '2') '1'
    
Lowest on the list is supporting abstracting over different recursion schemes
or supporting generic traversals and folds, though some basic support is
planned.

Finally, in at least some cases this can completely replace `GHC.Generics` and
may be a bit simpler. See `examples/Generics.hs` for an example of the
[`GHC.Generics` wiki example](http://www.haskell.org/haskellwiki/GHC.Generics)
ported to `shapely-data`. And for a nice view on the changes that were
required, do:

    git show 3a65e95 | perl /usr/share/doc/git/contrib/diff-highlight/diff-highlight


## Why not GHC.Generics?

The `GHC.Generics` representation has a lot of metadata and a complex
structure that can be useful in deriving default instances; more important to
us is to have a simple, canonical representation such that two types that
differ only in constructor names can be expected to have identical generic
representations. 

This supports APIs that are type-agnostic (e.g. a database library that returns
a generic `Product`, convertible later with `to`), and allows us to define
algebraic operations and composition & conversion functions.
