{-# LANGUAGE TypeFamilies , TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Shapely (
{- | 
/Issues and Limitations:/

  - 'massage' does not support mutually-recursive types and other more
    complicated recursion schemes (yet).

  - Adding meaningful structure to newtype wrapped types and terms with type
    application (e.g. @data T f a = T (f (T f a))@) should be handled with some
    type of 'Monoidal'-style inlining mechanism, but I'm not sure of the
    details of API design and implementation.

  - While all classes except 'Shapely' are considered closed, we don't do any
    tricks to enforce that in the API yet.

  - In fancier functions that use type equality (e.g. 'coerce'), types need to
    be unambiguous so type signatures are sometimes required.

  - type errors can be cryptic

  - Performance hasn't been tested at all yet.

 -}
      Product, Sum
    , Shapely(..)
    -- * Deriving Shapely instances automatically
    , deriveShapely

    , Isomorphic(..), CoercibleWith(..), Massageable(..)
    , ($$)
    ) where

import Data.Shapely.TH
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Normal.Massageable
import Data.Shapely.Normal.Coercible
import Data.Shapely.Utilities

import Control.Applicative

$(concat <$> mapM deriveShapely [ 
      ''(,,,,,,,,,,,,,,)
    , ''(,,,,,,,,,,,,,) 
    , ''(,,,,,,,,,,,,) 
    , ''(,,,,,,,,,,,) 
    , ''(,,,,,,,,,,) 
    , ''(,,,,,,,,,) 
    , ''(,,,,,,,,) 
    , ''(,,,,,,,) 
    , ''(,,,,,,) 
    , ''(,,,,,) 
    , ''(,,,,) 
    , ''(,,,) 
    , ''(,,) 
    , ''Ordering
    , ''Maybe
    , ''Bool
    ] )
