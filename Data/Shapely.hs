{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}  --these two for Isomorphic class
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   -- necessary for Shapely Generics instances
{-# LANGUAGE TypeOperators #-}       -- for our cons synonym
{-# LANGUAGE StandaloneDeriving #-}   -- these two for deriving AlsoNormal instances
{-# LANGUAGE UndecidableInstances #-}
module Data.Shapely (
{- | 
/Issues and Limitations:/  -- TODO remove some of these
.
  - Users can't express recursive structure of types without depending on this
    for 'AlsoNormal'
.
  - This is probably not useful for people using records as their interface,
    since they're treating product types as sets of terms where order is not
    significant
.
  - This new way of working with types presents some awkwardness in that the
    ordering of constructors in a 'Coproduct' type becomes significant (we
    are used to ordering of product terms being significant in pattern-
    matching).

/Sources, Inspiration, Prior Art/:  --TODO fill this out

  - ekmett's 'categories' package

  - HList
-}
      Product(..), Coproduct(..)
    , Shapely(..), AlsoNormal(..)
    , Isomorphic(..), coerce, massage
    , ($$)
    ) where

-- TODO: export TH functionality here
--import Data.Shapely.TH
import Data.Shapely.Compose.Classes
import Data.Shapely.Compose.Massageable


-- | Instances of the 'Shapely' class can be converted to and from a 'Normal'
-- representation, made up of @(,)@, @()@ and @Either@.
class Shapely a where
    -- | A @Shapely@ instances \"normal form\" representation, consisting of
    -- nested product, sum and unit types. Types with a single constructor will
    -- be given a 'Product' Normal representation, where types with more than
    -- one constructor will be 'Coproduct's. 
    --
    -- See the documentation for 'mkShapely', and the instances defined here
    -- for details.
    type Normal a
    toNorm :: a -> Normal a
    fromNorm :: Normal a -> a

-- | Note, the normal form for a tuple is not itself
instance Shapely (x,y) where
    type Normal (x,y) = (x,(y,()))
    toNorm (x,y) = (x,(y,()))
    fromNorm (x,(y,())) = (x,y)

-- Syntactically we can think of the type constructor (in this case (), but
-- usually, e.g. "Foo") becoming (), and getting pushed to the bottom of the
-- stack of its terms (in this case there aren't any).
instance Shapely () where  
    type Normal () = ()
    toNorm = id
    fromNorm = id

-- Here, again syntactically, both constructors Left and Right become `()`, and
-- we replace `|` with `Either` creating a sum of products.
instance Shapely (Either x y) where
    type Normal (Either x y) = Either (x,()) (y,())
    toNorm = let f = flip (,) () in either (Left . f) (Right . f)
    fromNorm = either (Left . fst) (Right . fst)
  --fromNorm = Sh.fanin (Left,(Right,())).

---- TODO: more instances by hand. Check their equivalents in tests of TH code by using a newtype wrapper. -----

-- TODO: alternate names: Another, ChildNormal, RecNormal
-- | A wrapper for recursive child occurences of a 'Normal'-ized type
newtype AlsoNormal a = Also { normal :: Normal a }
deriving instance (Show (Normal a))=> Show (AlsoNormal a)
deriving instance (Eq (Normal a))=> Eq (AlsoNormal a)
deriving instance (Ord (Normal a))=> Ord (AlsoNormal a)
deriving instance (Read (Normal a))=> Read (AlsoNormal a)


-- | Two types @a@ and @b@ are isomorphic if their 'Normal' representations are
-- the same.
class (Shapely a, Shapely b, Normal a ~ Normal b)=> Isomorphic a b
instance (Shapely a, Shapely b, Normal a ~ Normal b)=> Isomorphic a b

-- TODO -consider name change to avoid conflict with GHC 7.8 newtype 'coerce' function?
--      -make method of Isomorphic class?
-- | Convert a type @a@ to an isomorphic type @b@.
--
-- > coerce = fromNorm . toNorm
--
-- See 'massage' for a more powerful and flexible conversion function.
coerce :: (Isomorphic a b)=> a -> b
coerce = fromNorm . toNorm

-- | A \"fuzzy\" coerce function, supporting collapsing and re-ordering of
-- 'Coproduct' types, and treating 'Product's like sets (a la \"type-indexed
-- products\") in a sane manner.
--
-- See 'Massageable' for more details on how this conversion works.
--
-- > massage a = massageNormal $$ a
massage :: (Shapely a, Shapely b, Massageable (Normal a) (Normal b)) => a -> b
massage a = massageNormal $$ a

-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = fromNorm . f . toNorm
infixr 1 $$  --is this right?
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = fromNorm . f . toNorm

