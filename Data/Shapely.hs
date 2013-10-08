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
    , Isomorphic(..), coerce, Massageable(..)
    , ($$)
    ) where

-- TODO: export TH functionality here
--import Data.Shapely.TH
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Normal.Massageable



-- TODO -consider name change to avoid conflict with GHC 7.8 newtype 'coerce' function?
--      -make method of Isomorphic class?
-- | Convert a type @a@ to an isomorphic type @b@.
--
-- > coerce = from . to
--
-- See 'massage' for a more powerful and flexible conversion function.
coerce :: (Isomorphic a b)=> a -> b
coerce = from . to


-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = from . f . to
infixr 1 $$  --is this right?
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = from . f . to

