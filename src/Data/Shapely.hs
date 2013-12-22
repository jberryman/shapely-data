{-# LANGUAGE TypeFamilies , TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Shapely (
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
