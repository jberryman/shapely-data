{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- for nested Type families. We intend these to be closed anyway, so no biggie
{-# LANGUAGE FunctionalDependencies #-}  -- for Homogeneous
module Data.Shapely.Compose (
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
    , popr
    , Appendable(..)
    , Concatable(..)
    -- ** Convenience Type synonyms
    , (:*:), (:+:)
    -- * Operations on Products:
    , Uncurry(..)
    , Homogeneous(..)
    -- ** Convenience
    , (.++.), (|>), (<|), (<!)
    -- * Cartesian and CoCartesian-like
    -- ** Cartesian
    , Replicated(..)
    , Fanout(..)
    -- ** CoCartesian
    , Extract(..)
    , Fanin(..)
    ) where

import Data.Shapely
import Data.Shapely.Category
import Control.Applicative() -- Functor instances for (,) and Either

import Prelude hiding (replicate,concat,reverse,uncurry)
import qualified Prelude 


-- TODO?
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
--       - and, or, sum, product, maximum, minimum (NO: toList is sufficient for all of these folds)
--       - folds, maps, scans
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
-- -------



-- | A singleton inhabited 'Coproduct'. This is an intermediate type useful for
-- constructing Conproducts, and in our instances (see e.g. 'Tail')
newtype Only a = Only a

type instance NormalConstr (Only a) = Either
instance (Product t)=> Coproduct (Only t)

infixr :+:
infixr :*:
type (:*:) = (,)
type (:+:) = Either


-- TODO: consider making all Left parameters and results of sums explicitly () and (a,b)


-- ARE THESE ACTUALLY USEFUL??
type family Head xs
type family Tail xs
type instance Head (x,xs) = x
type instance Head (Either x xs) = x

type instance Tail (x,xs) = xs
type instance Tail (Either x (Either y ys)) = Either y ys 
type instance Tail (Either y ()) = Only ()
type instance Tail (Either y (a,b)) = Only (a,b)

type family Init xs
type instance Init (xN,()) = ()
type instance Init (x,(y,zs)) = (x, Init (y,zs)) 

type instance Init (Either x (Either y zs)) = x :< (Init (Either y zs))
type instance Init (Either x ()) = Only x
type instance Init (Either x (a,b)) = Only x

type family Last xs
type instance Last (x,()) = x
type instance Last (x,(y,zs)) = Last (y,zs) 

type instance Last (Either x (Either y zs)) = Last (Either y zs) 
type instance Last (Either x ()) = ()
type instance Last (Either x (a,b)) = (a,b)
type instance Last (Only b) = b

type family t :< ts
type instance x :< Only y = Either x y
type instance x :< Either y zs = Either x (Either y zs)
type instance x :< () = (x,())
type instance x :< (y,zs) = (x,(y,zs))

type family xs :> x
type instance () :> x = (x,())
type instance (x0,xs) :> x = (x0, xs :> x)

type instance Either x0 xs :> x = Either x0 (Tail (Either x0 xs) :> x)
type instance Only a :> b = Either a b -- TODO: insist on two instances for: b ~ () and b ~ (x,y)?


-- TODO: use instance defaulting here for these definitions after they are settled?
-- TODO: if we combine these methods into bigger classes, can we omit some constraints and simplify things?


-- | Note: @popl@ would be simply @id@.
--
-- > popr = swap . shiftr
popr :: (Symmetric (->) p, Shiftable t, ShiftedR t ~ p a b) => t -> p b a
popr = swap . shiftr


-- | Class supporting normal type equivalent to list @concat@ function, for
-- 'Coproduct's as well as 'Product's
class Concatable xs where
    type Concated xs
    concat :: xs -> Concated xs

instance Concatable () where
    type Concated () = ()
    concat = id

instance (Concatable yss, Appendable xs (Concated yss), Product (Concated yss))=> Concatable (xs,yss) where
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

-- TODO: restrict (Either x ys) to Coproduct?
instance (Concatable ess, Appendable (Either x ys) (Concated ess))=> Concatable (Either (Either x ys) ess) where
    type Concated (Either (Either x ys) ess) = Either x ys :++: Concated ess
    concat = append . fmap concat



-- | Reversing Products and Coproducts
class Reversable t where
    type Reversed t
    type Reversed t = Reversed (Tail t) :> Head t
    reverse :: t -> Reversed t

instance Reversable () where
    type Reversed () = ()
    reverse = id

instance Reversable (Only x) where
    type Reversed (Only x) = (Only x)
    reverse = id

instance (Reversable xs
         , Shiftable (x, Reversed xs)
         )=> Reversable (x,xs) where
    reverse = shiftl . fmap reverse 

instance (Reversable xs
         , Shiftable (Either x (Reversed xs))
         , (Tail (Either x (Reversed xs)) :> x)
           ~ (Reversed (Tail (Either x xs)) :> x) -- TODO improve this?
         )=> Reversable (Either x xs) where
    reverse = shiftl . fmap reverse 



-- | a class for shifting a sum or product left or right by one element, i.e. a
-- logical shift
class Shiftable t where
    shiftl :: t -> ShiftedL t
    shiftr :: t -> ShiftedR t
    
type ShiftedL t = Tail t :> Head t
type ShiftedR t = Last t :< Init t

swapFront :: Symmetric (->) p => p b (p a c) -> p a (p b c)
swapFront = associate . first swap . disassociate

instance Shiftable (x,()) where
    shiftl = id
    shiftr = id

-- TODO abstract these constraints better:
instance (Shiftable (y,zs)
        , Shiftable (x, zs)
        , ShiftedR (y,zs) ~ (Last (y, zs), Init (y, zs)) -- TODO replace with better constraint
        )=> Shiftable (x,(y,zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

instance Shiftable (Either a ()) where
    shiftl = swap
    shiftr = swap

instance Shiftable (Either a (x,y)) where
    shiftl = swap
    shiftr = swap

--- TODO: simplify these constraints:
instance (Shiftable (Either y zs)
        , Shiftable (Either x zs)
        , (Tail (Either x zs) :> x)
          ~ (Tail (Either y zs) :> x)
        , (Last (Either y zs) :< (x :< Init (Either y zs))) 
          ~ Either a0 (Either x c0)
        , (Last (Either y zs) :< Init (Either y zs))
          ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

-- | A class supporting an N-ary 'uncurry' operation for applying functions to
-- 'Normal' 'Product's of matching arity. e.g.
--
-- > uncurry (+) (1,(2,())) == 3
class Uncurry t r where
    -- | A function capable of consuming the product @t@ and producing @r@.
    type t :->-> r
    uncurry :: (t :->-> r) -> t -> r

-- | uncurry = const
instance Uncurry () r where
    type () :->-> r = r
    uncurry = const

instance (Uncurry bs r)=> Uncurry (a,bs) r where
    type (a,bs) :->-> r = a -> bs :->-> r
    uncurry f = Prelude.uncurry (uncurry . f)





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
--TODO: - our classes don't require the first constraint above
--      - how can we combine multiple clasues?

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
--
-- > 
(<!) :: x -> y -> (x,(y,()))
x <! y = (x,(y,()))



-- TODO: add this to a different class??
class Homogeneous a p | p -> a where
    toList :: p -> [a]

instance Homogeneous a () where
    toList () = []

instance (Homogeneous a bs)=> Homogeneous a (a,bs) where
    toList (a,bs) = a : toList bs



-- TODO it would be nice to be able to put these in the same class or define
-- `diag` in terms of `fanout`.

-- | "Fill" a product with an initial value. If the "length" of the resulting
-- product can't be inferred from context, provide a sype signature:
--
-- > truths = replicate True :: (Bool,(Bool,(Bool,())))
--
-- See also 'extract' for 'Coproduct's
class Replicated a as | as -> a where
    -- | Equivalent to @diag@ 
    replicate :: a -> as

instance Replicated a () where
    replicate _ = ()

instance (Replicated a as)=> Replicated a (a,as) where
    replicate a = (a,replicate a)

class Fanout a fs | fs -> a where
    type FannedOut fs
    -- | equivalent to @(&&&)@
    fanout :: fs -> (a -> FannedOut fs)

instance (Fanout a fs)=> Fanout a (a -> x,fs) where
    type FannedOut (a -> x, fs) = (x, FannedOut fs) 
    fanout (f,fs) a = (f a, fanout fs a)

instance Fanout a () where
    type FannedOut () = ()
    fanout () _ = ()



-- | Extract a value from a homogeneous sum.
--
-- See also 'replicate' for 'Product's.
class Product a=> Extract a as | as -> a where
    -- | generalizes @codiag@:
    extract :: as -> a

instance Extract () () where
    extract = id

instance (Product bs)=> Extract (a,bs) (a,bs) where
    extract = id

instance (Extract a as)=> Extract a (Either a as) where
    extract = either id extract


-- putting the product arg on LHS of the FannedIn func gives us more
-- flexibility (copord doesn't have to be strictly a sum of prods) but makes
-- inferrence more tricky (a user is probably more likely to be combining
-- polymorphic functions for application to a generated coproduct)
class Fanin s c | s -> c where
    type FannedIn s c
    -- | Apply an uncurried 'Product' of functions of type @x,y,z,etc -> c@ to a
    -- 'Coproduct' of @x, y, z@, yielding a @c@. E.g.
    --
    -- > let s' :: Either  (Int,())  ( Either  ()  ([Int],([Int],())) )
    -- >     s' = Left (1,()) 
    -- >  in fanin ((+1), (3, ((length .) . (++), ()))) s'  ==  2
    --
    -- This is like (@|||@) and can be thought of as a generalization of
    -- 'either'.
    fanin :: FannedIn s c -> s -> c

-- so we need to make products the base case for the final term of coproducts::
instance (Uncurry () c)=> Fanin () c where
    type FannedIn () c = ( () :->-> c , ())
    fanin = uncurry . fst

instance (Uncurry (x,xs) c)=> Fanin (x,xs) c where
    type FannedIn (x,xs) c = ((x,xs) :->-> c, ())
    fanin = uncurry . fst

instance (Fanin bs c, Uncurry a c)=> Fanin (Either a bs) c where
    type FannedIn (Either a bs) c = (a :->-> c, FannedIn bs c)
    fanin (f,fs) = either (uncurry f) (fanin fs)

{-  Could be simplified to....
instance (Fanin (Tail (Either a bs)) c, Uncurry a c)=> Fanin (Either a bs) c where
    type FannedIn (Either a bs) c = (a :->-> c, FannedIn (Tail (Either a bs)) c) -- this won't work
    fanin (f,fs) = either (uncurry f) (fanin fs) -- <- not quite

instance (Uncurry a c)=> Fanin (Only a) c where
    type FannedIn (Only a) c = (a :->-> c,())
    fanin (f,()) (Only a) = uncurry f a

...if we had a function like :: Either a (Either b c) -> Either a (Either b (Only c))

TODO: probably some of these other definitions could as well. e.g. 'Extract'
-}
