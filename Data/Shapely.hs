{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}  --these two for Isomorphic class
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   -- necessary for Shapely Generics instances
{-# LANGUAGE TypeOperators #-}       -- for our cons synonym
{-# LANGUAGE StandaloneDeriving #-}   -- these two for deriving AlsoNormal instances
{-# LANGUAGE UndecidableInstances #-}
-- TODO clean up above
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
import Data.Shapely.Classes
import Data.Shapely.Compose.Classes
import Data.Shapely.Compose.Massageable



-- TODO -consider name change to avoid conflict with GHC 7.8 newtype 'coerce' function?
--      -make method of Isomorphic class?
-- | Convert a type @a@ to an isomorphic type @b@.
--
-- > coerce = fromNorm . toNorm
--
-- See 'massage' for a more powerful and flexible conversion function.
coerce :: (Isomorphic a b)=> a -> b
coerce = fromNorm . toNorm

-- | A typed \"fuzzy\" coerce function, supporting collapsing and re-ordering of
-- 'Coproduct' types, and treating 'Product's like sets (a la \"type-indexed
-- products\") with re-ordering, and mapping equivalent recursive subterms.
--
-- See 'Massageable' for more details on how this conversion works.
massage :: (Shapely a, Shapely b, Massageable a b (Normal a) (Normal b)) => a -> b
massage a = let b = massageNormalRec (undefined `asTypeOf` a) (undefined `asTypeOf` b) $$ a
             in b


-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = fromNorm . f . toNorm
infixr 1 $$  --is this right?
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = fromNorm . f . toNorm

