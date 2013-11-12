{-# LANGUAGE MultiParamTypeClasses
 , TypeFamilies
 , FlexibleInstances, FlexibleContexts
 , OverlappingInstances -- for IsoNormal to support recursive subterms
 , UndecidableInstances -- for Isomorphic instance
 , ScopedTypeVariables  -- for Isomorphic instance (why isn't there a helper function in Data.Proxy?)
 #-}
module Data.Shapely.Normal.Isomorphic
    where

import Data.Shapely.Category
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Utilities

import Data.Proxy

-- TODO or name SpineTypes, or Spine
class (Product l)=> Structure l 
instance (Structure ts, Shapely t)=> Structure (Proxy t, ts)
instance Structure ()

-- separated, again, to contain OverlappingInstances


-- | Two types @a@ and @b@ are isomorphic, by this definition, if they have the
-- same number and ordering of constructors, and where 'Product' terms are
-- identical or a corresponding recursive @a@ and @b@.
class (Shapely a, Shapely b)=> Isomorphic a b where
    -- | Convert a possibly direct-recursive type @a@ to an isomorphic type
    -- @b@.
    --
    -- See 'massage' for a more powerful and flexible conversion function
    -- supporting direct recursion.
    coerce :: a -> b

instance (IsomorphicWith (Proxy a,()) a b)=> Isomorphic a b where
    coerce a = coerceWith (Proxy :: Proxy a, ()) a


-- TODO not really Isomorphic since we can't reverse a and b and keep ts
class (Structure ts, Shapely a, Shapely b)=> IsomorphicWith ts a b where
    -- | Convert a type @a@ to an isomorphic type @b@, where the 'Proxy' types
    -- in @ts@ define the recursive structure of @a@.
    coerceWith :: ts -> a -> b

instance (Shapely a, Shapely b, IsoNormal ts (Normal a) (Normal b))=> IsomorphicWith ts a b where
    coerceWith ts a = coerceNormalWith ts $$ a

-- ---------------- INTERNAL (for now)

class (Structure ts)=> IsoNormal ts na nb where
    coerceNormalWith :: ts -> na -> nb

instance (Structure ts)=> IsoNormal ts () () where
    coerceNormalWith _ = id
{-
instance (Isomorphic a b, IsoNormal a as bs)=> IsoNormal a (a,as) (b,bs) where  -- NOTE: `b` may ~ `a`
    coerceNormalWith a' (a,as) = (coerce a, coerceNormalWith a' as)
instance (b0 ~ b1, IsoNormal a as bs)=> IsoNormal a (b0,as) (b1,bs) where
    coerceNormalWith = fmap . coerceNormalWith
-}
-- replace with:
instance (IsoTerms ts ts x y, IsoNormal ts as bs)=> IsoNormal ts (x,as) (y,bs) where
    coerceNormalWith ts (x,as) = (coerceTermWith ts ts x, coerceNormalWith ts as)

instance (IsoNormal a as as', IsoNormal a bs bs')=> IsoNormal a (Either as bs) (Either as' bs') where
    coerceNormalWith a' = bimap (coerceNormalWith a') (coerceNormalWith a')


class (Structure tsOrig, Structure tsBeingChecked)=> IsoTerms tsBeingChecked tsOrig a b where
    coerceTermWith :: tsBeingChecked -> tsOrig -> a -> b

instance (Structure tsOrig)=> IsoTerms () tsOrig a a where
    coerceTermWith _ _ = id

instance (Structure ts, IsomorphicWith tsOrig a b)=> IsoTerms (Proxy a, ts) tsOrig a b where -- note: `b` may ~ `a`
    coerceTermWith _ = coerceWith

instance (Structure ts, Shapely x, IsoTerms ts tsOrig a b)=> IsoTerms (Proxy x, ts) tsOrig a b where
    coerceTermWith = coerceTermWith . snd

