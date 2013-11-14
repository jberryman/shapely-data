{-# LANGUAGE MultiParamTypeClasses
 , TypeFamilies
 , FlexibleInstances, FlexibleContexts
 , OverlappingInstances -- for IsoNormal to support recursive subterms
 , UndecidableInstances -- for Isomorphic instance
 , FunctionalDependencies -- for IsOfBaseType
 , ScopedTypeVariables -- for bool passing trick in 2nd IsoTerms instance
 , PolyKinds
 #-}
module Data.Shapely.Normal.Isomorphic
    where

-- separated, again, to contain OverlappingInstances

import Data.Shapely.Category
import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Utilities
import Data.Shapely.Bool

import Data.Shapely.Spine




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
    coerce a = coerceWith (proxyTypeOf a, ()) a


-- TODO not really Isomorphic since we can't reverse a and b and keep ts
class (SpineOf ts, Shapely a, Shapely b)=> IsomorphicWith ts a b where
    -- | Convert a type @a@ to an isomorphic type @b@, where the 'Proxy' types
    -- in @ts@ define the recursive structure of @a@.
    coerceWith :: ts -> a -> b

instance (Shapely a, Shapely b, IsoNormal ts (Normal a) (Normal b))=> IsomorphicWith ts a b where
    coerceWith ts a = coerceNormalWith ts $$ a

-- ---------------- INTERNAL (for now)

class (SpineOf ts)=> IsoNormal ts na nb where
    coerceNormalWith :: ts -> na -> nb

instance (SpineOf ts)=> IsoNormal ts () () where
    coerceNormalWith _ = id

instance (IsoTerms ts ts x y, IsoNormal ts as bs)=> IsoNormal ts (x,as) (y,bs) where
    coerceNormalWith ts (x,as) = (coerceTermWith ts ts x, coerceNormalWith ts as)

instance (IsoNormal a as as', IsoNormal a bs bs')=> IsoNormal a (Either as bs) (Either as' bs') where
    coerceNormalWith a' = bimap (coerceNormalWith a') (coerceNormalWith a')


class (SpineOf tsOrig, SpineOf tsBeingChecked)=> IsoTerms tsBeingChecked tsOrig a b where
    coerceTermWith :: tsBeingChecked -> tsOrig -> a -> b

instance (SpineOf tsOrig)=> IsoTerms () tsOrig a a where
    coerceTermWith _ _ = id

-- THESE TWO:
{-
instance (SpineOf ts, IsomorphicWith tsOrig a b)=> IsoTerms (Proxy a, ts) tsOrig a b where -- note: `b` may ~ `a`
    coerceTermWith _ = coerceWith

instance (SpineOf ts, Shapely x, IsoTerms ts tsOrig a b)=> IsoTerms (Proxy x, ts) tsOrig a b where
    coerceTermWith = coerceTermWith . snd
    -}
instance (SpineOf (Proxy x, ts), SpineOf tsOrig
         , IsOfBaseType x a bool, IsoTermFoo bool ts tsOrig a b
         )=> IsoTerms (Proxy x, ts) tsOrig a b where
    coerceTermWith = coerceTermWithFoo (Proxy :: Proxy bool) . snd

class IsoTermFoo (bool :: *) ts tsOrig a b where
    coerceTermWithFoo :: Proxy bool -> ts -> tsOrig -> a -> b

-- a was in spine
instance (IsomorphicWith tsOrig a b)=> IsoTermFoo True ts tsOrig a b where
    coerceTermWithFoo _ _ = coerceWith
-- a is not part of spine, try tail of spine list:
instance (IsoTerms ts tsOrig a b, SpineOf ts, SpineOf tsOrig)=> IsoTermFoo False ts tsOrig a b where
    coerceTermWithFoo _ = coerceTermWith


class IsOfBaseType t tab (b :: *) | t tab -> b -- NOTE (b :: *) fixed same issue we're seeing in Data.Shapely.Spine.BareType
instance IsOfBaseType t t True
instance IsOfBaseType (t a) (t a) True
instance (IsOfBaseType t ta bool)=> IsOfBaseType t (ta b) bool
instance false ~ False => IsOfBaseType t x false
