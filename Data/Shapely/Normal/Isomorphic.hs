{-# LANGUAGE MultiParamTypeClasses
 , TypeFamilies
 , FlexibleInstances, FlexibleContexts
 , UndecidableInstances -- for Isomorphic instance
 , OverlappingInstances -- for IsoNormal to support recursion
 #-}
module Data.Shapely.Normal.Isomorphic
    where

import Data.Shapely.Category
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes

-- separated, again, to contain OverlappingInstances


-- | Two types @a@ and @b@ are isomorphic if...
class (Shapely a, Shapely b)=> Isomorphic a b where
    -- TODO -consider name change to avoid conflict with GHC 7.8 newtype 'coerce' function?
    -- | Convert a type @a@ to an isomorphic type @b@.
    --
    -- See 'massage' for a more powerful and flexible conversion function.
    coerce :: a -> b

instance (IsoNormal a (Normal a) (Normal b), Shapely a, Shapely b)=> Isomorphic a b where
    coerce a = coerceNormalOf a $$ a
                where ($$) f = from . f . to -- TODO move definition from Data.Shapely

class IsoNormal a na nb where
    coerceNormalOf :: a -> na -> nb

instance IsoNormal a () () where
    coerceNormalOf _ = id

instance (Isomorphic a b, IsoNormal a as bs)=> IsoNormal a (a,as) (b,bs) where  -- NOTE: `b` may ~ `a`
    coerceNormalOf a' (a,as) = (coerce a, coerceNormalOf a' as)
instance (b0 ~ b1, IsoNormal a as bs)=> IsoNormal a (b0,as) (b1,bs) where
    coerceNormalOf = fmap . coerceNormalOf

instance (IsoNormal a as as', IsoNormal a bs bs')=> IsoNormal a (Either as bs) (Either as' bs') where
    coerceNormalOf a' = bimap (coerceNormalOf a') (coerceNormalOf a')
