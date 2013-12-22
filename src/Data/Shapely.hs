{-# LANGUAGE TypeFamilies , TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Shapely (
{- |
You can generate a 'Shapely' instance for custom types with, e.g.

> {-# LANGUAGE TemplateHaskell #-}
> data L a = Snoc (L a) a | Lin
> deriveShapely ''L

Then you can use 'coerce' or 'massage' to convert between types, or see the
functions in "Data.Shapely.Normal" for functions for composing and transforming
'Normal' form types, including algebraic operations and coversion functions.
 -}
      Shapely(..)
    , Product, Sum
    -- * Deriving Shapely instances automatically
    , deriveShapely
    -- * Typed conversion functions
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
