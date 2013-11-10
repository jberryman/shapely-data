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
-- TODO: move this to the .cabal file
{- | 
/Issues and Limitations:/
.
  - Users can't express recursive structure of types in their own code without
    depending on this package for 'AlsoNormal'.
.
  - 'massage' does not support mutually-recursive types and other more
    complicated recursion schemes (yet).

  - Adding meaningful structure to newtype wrapped types and terms with type
    application (e.g. @data T f a = T (f (T f a))@) should be handled with some
    type of 'Monoidal'-style inlining mechanism, but I'm not sure of the
    details of API design and implementation.

  - While all classes except 'Shapely' are considered closed, we don't do any
    tricks to enforce that in the API yet.

  - Performance hasn't been tested at all yet.

 -}
      Product(..), Coproduct(..)
    , Shapely(..), AlsoNormal(..)
    -- * Deriving Shapely instances automatically
    , deriveShapely

    , Isomorphic(..), Massageable(..)
    , ($$)
    ) where

import Data.Shapely.TH
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Normal.Massageable
import Data.Shapely.Normal.Isomorphic


-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = from . f . to
infixr 1 $$  --TODO check is this right?
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = from . f . to

