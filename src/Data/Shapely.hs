{-# LANGUAGE TypeFamilies , TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Shapely (
{- |
You can generate a 'Shapely' instance for custom types by adding a @LANGUAGE
TemplateHaskell@ pragma, and doing e.g.

> data L a = Snoc (L a) a | Lin
> deriveShapely ''L

Then you can use 'coerce' or 'massage' to convert between types, or see the
functions in "Data.Shapely.Normal" for functions for composing and transforming
'Normal' form types, including algebraic operations and coversion functions.
 -}
      Shapely(..)
    -- ** Products and Sums
{- |
These classes and all the others besides 'Shapely' should be treated as closed.
The instances here document what we mean by \"product\" and \"sum\".
 -}
    , Product, Sum
    -- * Deriving Shapely instances automatically
    , deriveShapely
    -- * Typed conversion functions
    , Isomorphic(..), CoercibleWith(..), Massageable(..)
    -- * Utilities
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
