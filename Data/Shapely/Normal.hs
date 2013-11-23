{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- for nested Type families. We intend these to be closed anyway, so no biggie
{-# LANGUAGE FunctionalDependencies #-}
-- NO OVERLAPPING INSTANCES HERE, PLEASE
module Data.Shapely.Normal (
{- |
Functions for composing and modifying our 'Normal' form types.

These take their names from the familiar functions in Data.List and Prelude,
but are given more general forms, sometimes loosely after the constructions in
Edward Kmett's "categories" package. There are probably many improvements and
additions possible here.

/NOTE/: The structure of the classes, type functions, and class constraints
here are likely to change a lot, however the names of individual functions and
methods and their use should be stable, so hopefully there will be few
compatibility issues when this module is improved.
-}
      Only(..)

    -- * Operations on 'Product's and 'Coproduct's
    , Reversable(..)
    , Shiftable(..)
    , viewr
    , Appendable(..)
    , Concatable(..)
    -- ** Fanned Application
    , Fans(..)
    , constructorsOfNormal
    -- ** Convenience Type synonyms
    , (:*:), (:*!), (:+:)

    -- * Operations on Products
    , List(..)
    , Extract(..)
    -- ** Composition & Construction Convenience Operators
    , (.++.), (|>), (<|), (<!)

    -- * Product and Coproduct Conversions
    , MassageableNormal(..)
    ) where

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Classes
import Data.Shapely.Normal.Massageable
import Data.Shapely.Normal.FannedApplication
import Control.Applicative() -- Functor instances for (,) and Either

import Prelude hiding (replicate,concat,reverse, map)
import qualified Prelude 


-- TODO:
--      - fix Concatable/Appendable, add better Monoidal class
--      - implement FunctorOn
--      - create a length-indexed list (opaque for safety) that is, perhaps
--        Foldable/Traversable and can be converted from and back into a Product.
--        Maybe replace toList with Foldable.toList
--
--      - look into type families w/ coincident overlap for SHapely.Bool module
--           http://typesandkinds.wordpress.com/2013/04/29/coincident-overlap-in-type-families/
--           - also try to see if / where this might improve inferrence
--      - make sure we know exactly how OverlappingInstances and
--        UndecidableInstances are working and that they're safe here
--          - double check all uses (remove / recompile / assess)
--      - derive Shapely instances for all built-in types
--      - documentation:
--          - make note about plans for AlsoNormal on that constructor
--          - fix 'limitations' section
--          - make a note about future plans for inlining.
--      - implement thorough tests for 'massage', and TH-derived stuff.
--          - especially recursion, which we haven't tested well
--      - take last look at easy construction of normal-form types
--      - create some examples that re-create GHC generics motivation
--      - finalize exports, modules, finish cabal file w/ proper docs & motivation
--
--   v0.2:
--      - move to closed type families, look at replacing OverlappingInstances
--        with these by using families for type equality
--          - use closed type fams in proxy-kindness too
--      - use some scheme to close type classes (maybe closed type fams will help)
--      - freeze 'massage' behavior
--      - support inlining, and "templates" defining structure. Have NormalR use this
--      - maybe: 
--          - type-indexed 'factor' (and maybe 'distrivbute')
--          - incorporate TypeNat stuff (for specifying length and constructor number)
--
--    sometime:
--      - function that does a series expansion (up to _nth) of a recursive type
--      - read up about "row types"
--      - see if where and if we can make interesting use of DataKinds
--          - replacing didactic classes with "kindly-typed" structures?
--      - read "Small induction recursion..." paper
--      - check out "multirec" approach to recursive structure, and http://mainisusuallyafunction.blogspot.com/2010/12/type-level-fix-and-generic-folds.html
--      - consider some scheme (from proxy-kindness) for optionally "guarding"
--         ambiguous/polymorphic terms coming from parameterized types in
--         functions that make use of type equality (e.g. massage)
--
-- OTHER FUNCTIONS:
--   
--   - straighten :: e.g. ((a,b),(c,(d,e))) -> (a,(b,(c,(d,(e,()))))) -- requires closed type family (or overlapping instances)
--   - factor & distribute, (+ type-indexed variants)
--   - a recursive, monoid-style `zipWith` (i.e. an `mappend` that does something useful recursively)?
--
--   taking a numeric arg (implement with sum/product "templates"/peano numbers)
--     - length type func. (with type nats?)
--     - splitAt  (product take/drop in terms of this)
--     - popi (or factori?). product (!!) in terms of this
--     - replicate
--
--   complex, maybe not useful:
--     - intersperse
--     - intercalate
--     - transpose
--
--   products only: (USEFUL; DO LATER)
--     - zips ... e.g.:  zip ( (1,(2,())) , ( (True,(False,())) ,  ( ('a',('b'('c',()))) , ()))) == ....
--         but should we (can we) deal with differing-length prods like zip?  ... type Zipped () (a,b) = () , Zipped (a,b) () = () , Zipped (a,bs) (x,ys) = ( (a,(x,())) , Zipped bs ys) ...or for prods of prods we probably will be wanting a fold on products... oy
--         note also: above should be named "transpose"
--     Homogeneous only:
--       - and, or, sum, product, maximum, minimum, folds (NO: toList is sufficient for all of these folds)
--       - maps, scans
--       - sort/insert ?
--
-- -------
-- FUTURE TODOs:
--   -figure out rest of equivalents of categories' Control.Category.Cartesian. see notes re. replicate & sum/product numerals (templates?)
--   -closed TypeFamilies for simpler instances?
--   -type-level naturals for e.g. 'length;-like functions
--   -explore relationship to:
--     -lens
--     -vinyl
--     -Arrow
--     - "lifted" programming with -XDataKinds
-- -------




infixr 5 :+:
infixr 6 :*:
infixr 6 :*!
type (:+:) = Either
type (:*:) = (,)
type (x :*! y) = (x,(y,()))


-- TODO: use instance defaulting here for these definitions after they are settled?
-- TODO: if we combine these methods into bigger classes, can we omit some constraints and simplify things?


-- | Note: @viewl@ would be simply @id@.
--
-- > viewr = swap . shiftr
viewr :: (Symmetric (->) p, Shiftable t, ShiftedR t ~ p a b) => t -> p b a
viewr = swap . shiftr


-- TODO: or name this class "higher-order normal" or something?
--       we should be able to do 'append' here with a match on ((x,xs),xss) .. ((),xss)... etc.
-- | Class for flattening a 'Product' of 'Product's, or a nested sum of
-- 'Coproduct's.
class Concatable xs where
    type Concated xs
    -- | A generalization of 'Data.List.concat' for sums and products
    concat :: xs -> Concated xs

instance Concatable () where
    type Concated () = ()
    concat = id

-- | combine a product of products
instance (Concatable yss, Appendable xs (Concated yss)
         , Product xs, Product (Concated yss)
    )=> Concatable (xs,yss) where
    type Concated (xs,yss) = xs :++: Concated yss
    concat = append . fmap concat

-- These are the two possible leaf Coproducts at the Last position of
-- our Either spine:
instance Concatable (Either () es) where
    type Concated (Either () es) = (Either () es)
    concat = id

instance Concatable (Either (x,ys) es) where
    type Concated (Either (x,ys) es) = (Either (x,ys) es)
    concat = id

-- | Note here that @xs@ is not a 'Coproduct', since it is a sum of sums.
instance (Concatable ess, Appendable (Either x ys) (Concated ess)
         , Coproduct (Either x ys), Coproduct (Concated ess)
    )=> Concatable (Either (Either x ys) ess) where
    type Concated (Either (Either x ys) ess) = Either x ys :++: Concated ess
    concat = append . fmap concat



-- | Reversing 'Products' and 'Coproduct's
class Reversable t where
    type Reversed t
    type Reversed t = Reversed (Tail t) :> Head t
    reverse :: t -> Reversed t

instance Reversable () where
    type Reversed () = ()
    reverse = id

instance (Reversable xs
         , Shiftable (x, Reversed xs)
         )=> Reversable (x,xs) where
    reverse = shiftl . fmap reverse 

instance Reversable (Either x ()) where
    type Reversed (Either x ()) = Either () x
    reverse = swap
instance Reversable (Either x (a,b)) where
    type Reversed (Either x (a,b)) = Either (a,b) x
    reverse = swap

instance ( xs ~ Either y zs  -- for readability
         , Reversed (Either x xs) 
            ~ ShiftedL (Either x (Reversed xs))
         , Reversable xs
         , Shiftable (Either x (Reversed xs))
    )=> Reversable (Either x (Either y zs)) where
    reverse = shiftl . fmap reverse 


-- TODO: make instance for (), making ShiftedL/R assoc types?
-- | a class for shifting a sum or product left or right by one element, i.e. a
-- logical shift
class Shiftable t where
    shiftl :: t -> ShiftedL t
    shiftr :: t -> ShiftedR t
    
type ShiftedL t = Tail t :> Head t
type ShiftedR t = Last t :< Init t

instance Shiftable (x,()) where
    shiftl = id
    shiftr = id

instance (Shiftable (y,zs)
        , Shiftable (x, zs)
        , ShiftedR (y,zs) ~ (Last (y, zs), Init (y, zs))
        )=> Shiftable (x,(y,zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

instance Shiftable (Either a ()) where
    shiftl = swap
    shiftr = swap
instance Shiftable (Either a (x,y)) where
    shiftl = swap
    shiftr = swap

instance (Shiftable (Either y zs)
        , Shiftable (Either x zs)
        , xs ~ (Either y zs) -- for readability
        -- TODO simplify these
        , Tail (Either x zs) ~ Tail xs
        , (Last xs :< (x :< Init xs)) ~ Either a0 (Either x c0)
        , (Last xs :< Init xs)        ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr



-- TODO: rethink this and Concatable
--         - This is a bit redundant in light of Concatable, though we'd have
--            to move the functionality here up there
--         - in that case we want to use perhaps :**:, or something with an
--            arrow a.la list comprehensions?
--       make room for a "multiply" function for products and coproducts. 
--          - NOTE: cartesian product of coproducts
--          - Then also consider making (|>) follow the Monoidal style and also
--             work on coproducts (this is multiplication)
--              - then also look at the :> type function...
--

-- | A @(++)@-like append operation on 'Product's and 'Coproduct's. See also
-- ('.++.'). e.g.
--
-- > append ( (1,(2,())) , (3,(4,())) )  ==  (1,(2,(3,(4,()))))
--
-- > s :: Either (Bool, ()) (Either (Char, ()) (Either (Int, ()) ()))
-- > s = append ( Right $ Left (1,()) :: Either  (Either (Bool,()) (Char,()))  (Either (Int,()) ()) )
class (NormalConstr xs ~ NormalConstr ys) => Appendable xs ys where
    type xs :++: ys   
    append :: (t ~ NormalConstr xs)=> t xs ys -> (xs :++: ys)

instance (Product us)=> Appendable () us where
    type () :++: us = us 
    append = snd

instance (Product us, Appendable ts us)=> Appendable (a,ts) us where
    type (a,ts) :++: us = (a, ts :++: us) 
    append = fmap append . associate

instance (Coproduct us)=> Appendable (Either a ()) us where
    type Either a () :++: us = Either a (Either () us)
    append = associate
instance (Coproduct us)=> Appendable (Either a (b,c)) us where
    type Either a (b,c) :++: us = Either a (Either (b,c) us)
    append = associate

instance (Appendable (Either b ts) us)=> Appendable (Either a (Either b ts)) us where
    type Either a (Either b ts) :++: us = Either a (Either b ts :++: us)
    append = fmap append . associate



infixr 5 .++.
-- | A convenience operator for concating two product types.
-- 
-- > (.++.) = curry append
(.++.) :: (Product xs, Product ys, Appendable xs ys)=> xs -> ys -> xs :++: ys
(.++.) = curry append

infixl 5 |>
infixr 5 <| 
infixr 5 <!
-- | A convenience operator for appending an element to a product type.
-- 'Shiftable' generalizes this operation.
--
-- > xs |> x = shiftl (x,xs)
(|>) :: (Product xs, Shiftable (x,xs))=> xs -> x -> (xs :> x)
xs |> x = shiftl (x,xs)

-- | A left push for Products.
--
-- > (<|) = (,)
(<|) ::  (Product xs)=> x -> xs -> (x,xs)
(<|) = (,)

-- | Convenience function for combining 'Product' terms, with ('<|'), e.g.
-- @0 <| 1 <| 2 <! 3@
--
-- > x <! y = (x,(y,()))
(<!) :: x -> y -> (x,(y,()))
x <! y = (x,(y,()))

-- | > constructorsOfNormal = 'unfanin' id
--
-- See also 'constructorsOf'. E.g.
--
-- > constructorsOfNormal ('a',('b',())) 'x' 'y'  ==  ('x',('y',()))
constructorsOfNormal :: (Fans r r)=> r -> (r :=>-> r)
constructorsOfNormal r = unfanin (id . (`asTypeOf` r))


-- | A class for homogeneous products with terms all of type @a@.
class (Product as)=> List a as | as -> a where
    -- | Convert a homogeneous product to a list.
    toList :: as -> [a]

    -- | "Fill" a product with an initial value. If the size of the resulting
    -- product can't be inferred from context, provide a sype signature:
    --
    -- > truths = replicate True :: (Bool,(Bool,(Bool,())))
    --
    -- An n-ary @codiag@. See also 'extract' for 'Coproduct's
    replicate :: a -> as

  -- map :: (bs `OfLength` as, List b bs)=> (a -> b) -> as -> bs --TODO

instance List a () where
    toList () = []
    replicate _ = ()

instance (List a as)=> List a (a,as) where
    toList (a,as) = a : toList as
    replicate a = (a,replicate a)



-- | Extract the value from a homogeneous sum.
--
-- See also 'replicate' for 'Product's.
class Product a=> Extract a as | as -> a where
    -- | an n-ary @codiag@:
    extract :: as -> a

instance (Product a)=> Extract a (Only a) where
    extract = just

instance (EitherTail as, Extract a (AsTail as))=> Extract a (Either a as) where
    extract = eitherTail id extract
