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
but are given more general forms after the constructions in Edward Kmett's
"categories" package. There are probably many improvements and additions
possible here.

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
    , Fanin(..), Fanout(..)
    -- ** Convenience Type synonyms
    , (:*:), (:*!), (:+:)

    -- * Operations on Products
  --, Uncurry(..) DEAD
    , List(..)
    -- ** Composition & Construction Convenience Operators
    , (.++.), (|>), (<|), (<!)

    -- * Cartesian and CoCartesian-like
    -- ** Cartesian
  --, Replicated(..) -- MOVED / DEAD
  --, Fanout(..)  --MOVED
    -- ** CoCartesian
    , Extract(..)
  --, Fanin(..) --MOVED

    -- * Product and Coproduct Conversions
    , MassageableNormal(..)
    ) where

import Data.Shapely
import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Classes
import Data.Shapely.Normal.Massageable
import Control.Applicative() -- Functor instances for (,) and Either

import Prelude hiding (replicate,concat,reverse, map)
import qualified Prelude 


-- TODO?
--      - add the TIP-style function, re-order exports grouping 'extract'-style functions
--      - replace pattern matching on Only with `just`
--      - fix docs
--
--      - implement TH stuff, derive instances for all built-in types
--      - implement thorough tests for 'massage', and TH-derived stuff.
--          - especially recursion, which we haven't tested well
--      - take last look at easy construction of normal-form types
--      - create some examples that re-create GHC generics motivation
--      - use some scheme to close type classes
--         - figure out exports
--
--   v0.2:
--      - freeze 'massage' behavior
--      - incorporate TypeNat stuff (for specifying length and constructor number)
--
--
-- OTHER FUNCTIONS:
--   
--   - factor
--      e.g. Either (a,bs) (a,cs) --> (a, Either bs cs) ... except several more type funcs required for this
--        type families we'd probably need here would be easier with a higher-order Mapped type family, but that's not allowed. Maybe the answer to that is here: http://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/
--   - distribute
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
infixr 6 :*!  -- TODO better fixity?
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


-- | Class supporting normal type equivalent to list @concat@ function, for
-- 'Coproduct's as well as 'Product's.
class Concatable xs where
    type Concated xs
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


-- TODO: make instance for (), making ShiftedL/R assoc types
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
        -- TODO simplify these
        , Tail (Either x zs) ~ Tail (Either y zs)
        , (Last (Either y zs) :< (x :< Init (Either y zs))) 
          ~ Either a0 (Either x c0)
        , (Last (Either y zs) :< Init (Either y zs))
          ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr


-- | A class for applying a structure to a 'Product' or 'Coproduct' @t@,
-- yielding an @r@.
class Fanin t r where
    -- | A structure capable of consuming the terms @t@ and producing @r@.
    type t :->-> r
    fanin :: (t :->-> r) -> t -> r

-- | > fanin = const
instance Fanin () r where
    type () :->-> r = r
    fanin = const

-- | n-ary @uncurry@ on 'Product's, for instance, e.g.
--
-- > fanin (+) (1,(2,())) == 3
instance (Fanin bs r)=> Fanin (a,bs) r where
    type (a,bs) :->-> r = a -> bs :->-> r
    fanin f = uncurry (fanin . f)

-- | n-ary @fanin@ on 'Coproduct's
--
-- > let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >  in fanin ((+1), (3, (foldr (+), ()))) s  ==  15
instance (Fanin (Tail (Either a bs)) r, EitherTail bs, Fanin a r)=> Fanin (Either a bs) r where
    type Either a bs :->-> r = (a :->-> r, Tail (Either a bs) :->-> r)
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)

instance (Fanin a r)=> Fanin (Only a) r where
    type Only a :->-> r = (a :->-> r, ())
    fanin (f,()) = fanin f . just


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
infixr 5 <! -- TODO: fixity?
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



-- | Apply a 'Product' or 'Coproduct' structure @fs@ to the term @s@ yielding a
-- product or coproduct @FannedOut fs@.
--
-- See also 'Fanin'.
class Fanout s fs | fs -> s where
    type FannedOut fs
    fanout :: fs -> (s -> FannedOut fs)

instance Fanout s () where
    type FannedOut () = ()
    fanout = const

-- ...and this is the result of @uncurry@ with arrows reversed.
-- | an n-ary @(&&&)@
instance (Fanout s fs)=> Fanout s (s -> x,fs) where
    type FannedOut (s -> x, fs) = (x, FannedOut fs) 
    fanout (f,fs) s = (f s, fanout fs s)

-- @fs@ is what we need to get @r -> E a b@, which is the result of @fanin@
-- with the arrow reversed. I'm not sure what to call this.
-- | e.g. 
--
-- > fanout (Left ((+1),()) :: Either (Int -> Int,()) (Int -> Bool,())) 1  ==  Left (2,())
instance (Fanout s fs, Fanout s fss)=> Fanout s (Either fs fss) where
    type FannedOut (Either fs fss) = Either (FannedOut fs) (FannedOut fss)
    fanout = either ((Left .) . fanout) ((Right .) . fanout) -- NOTE eitherTail/Only instance unnecessary


-- | Extract the value from a homogeneous sum.
--
-- See also 'replicate' for 'Product's.
class Product a=> Extract a as | as -> a where
    -- | an n-ary @codiag@:
    extract :: as -> a

instance (Product a)=> Extract a (Only a) where
    extract = just

instance (EitherTail as, Extract a (Tail (Either a as)))=> Extract a (Either a as) where
    extract = eitherTail id extract


-- Helpers for recursion on coproducts:
class EitherTail b where
    eitherTail :: (a -> c) -> (Tail (Either a b) -> c) -> Either a b -> c

instance EitherTail () where
    eitherTail f g = either f g . fmap Only

instance EitherTail (x,y) where
    eitherTail f g = either f g . fmap Only

instance EitherTail (Either x y) where
    eitherTail = either

{- NOTE initially tried this to avoid needing to define identical Coproduct
 - base cases for (a,b) and (), but it made instance constraint very noisey.
 - Not worth it.
-- map onto Tail of a Coproduct, yielding a Coproduct, and reconstructing. 
class (Coproduct b')=> FmapTail b' where
    fmapTail :: (EitherTail b)=> (Tail (Either a b) -> b') -> Either a b -> (a :< b')

instance (Coproduct (Either x y))=> FmapTail (Either x y) where
    fmapTail f = eitherTail Left (Right . f)

instance (Product x)=> FmapTail (Only x) where
    fmapTail f = eitherTail Left (Right . just . f)
    -}
