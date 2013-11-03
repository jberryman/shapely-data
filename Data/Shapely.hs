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
/Issues and Limitations:/
.
  - Users can't express recursive structure of types in their own code without
    depending on this package for 'AlsoNormal'.
.
  - 'massage' does not support mutually-recursive types and other more
    complicated recursion schemes (yet).

  - We don't know how to create an interesting \"normal\" form structure for
    fancy things like: @data T f a = T (f (T f a))@.

  - Performance hasn't been tested at all.

 -}
      Product(..), Coproduct(..)
    , Shapely(..), AlsoNormal(..)
    -- * Deriving Shapely instances automatically
    , deriveShapely

    , Isomorphic(..), coerce, Massageable(..)
    , ($$)
    ) where

import Data.Shapely.TH
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

