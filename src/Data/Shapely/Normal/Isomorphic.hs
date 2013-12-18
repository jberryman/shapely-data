{-# LANGUAGE MultiParamTypeClasses
 , TypeFamilies
 , FlexibleInstances, FlexibleContexts
 , OverlappingInstances -- for IsomorphicNormalWith to support recursive subterms
 , UndecidableInstances -- for Isomorphic instance
 , FunctionalDependencies -- for IsOfBaseType
 , ScopedTypeVariables -- for bool passing trick in 2nd IsoTerms instance
 , PolyKinds, DataKinds
 #-}
module Data.Shapely.Normal.Isomorphic
    where

-- separated, again, to contain OverlappingInstances

import Data.Shapely.Category
import Data.Shapely.Classes
import Data.Shapely.Utilities

import Data.Shapely.Spine
import Data.Proxy.Kindness


-- TODO Isomorphic and IsomorphicWith classes required? Or just make poymorphic functions

-- | Two types @a@ and @b@ are isomorphic, by this definition, if they have the
-- same number and ordering of constructors, and where 'Product' terms are
-- identical or a corresponding recursive @a@ and @b@.
class (Shapely a, Shapely b)=> Isomorphic a b where
    -- | Convert a possibly direct-recursive type @a@ to an isomorphic type
    -- @b@. This is defined:
    --  
    -- > coerce a = 'coerceWith' ('unappliedOf' a, ()) a
    --
    -- See 'massage' for a more powerful and flexible conversion function
    -- supporting direct recursion.
    coerce :: a -> b

-- | We use 'Unapplied' to recurse on all occurrences of the base type @t@,
-- regardless of its parameters.
instance (Unapplied (Proxy txy) t, IsomorphicWith (Proxy t,()) txy b)=> Isomorphic txy b where
    coerce a = coerceWith (unappliedOf a, ()) a


-- TODO not really Isomorphic since we can't reverse a and b and keep ts
-- | TODO
class (SpineOf ts, Shapely a, Shapely b)=> IsomorphicWith ts a b where
    -- | Convert a type @a@ to an isomorphic type @b@, where the 'Proxy' types
    -- in @ts@ define the recursive structure of @a@. See 'SpineOf'. 
    --
    -- These terms will be converted to the target type when they appear as
    -- top-level product terms or in nested 'Functor' applications.
    coerceWith :: ts -> a -> b

instance (Shapely a, Shapely b, IsomorphicNormalWith ts (Normal a) (Normal b))=> IsomorphicWith ts a b where
    coerceWith ts a = coerceNormalWith ts $$ a


-- ---------------- INTERNAL (for now)

class (SpineOf ts)=> IsomorphicNormalWith ts na nb where
    coerceNormalWith :: ts -> na -> nb

instance (SpineOf ts)=> IsomorphicNormalWith ts () () where
    coerceNormalWith _ = id

instance (IsoTerms ts ts x y, IsomorphicNormalWith ts as bs)=> IsomorphicNormalWith ts (x,as) (y,bs) where
    coerceNormalWith ts (x,as) = (coerceTermWith ts ts x, coerceNormalWith ts as)

instance (IsomorphicNormalWith a as as', IsomorphicNormalWith a bs bs')=> IsomorphicNormalWith a (Either as bs) (Either as' bs') where
    coerceNormalWith a' = bimap (coerceNormalWith a') (coerceNormalWith a')



class (SpineOf tsOrig, SpineOf tsBeingChecked)=> IsoTerms tsBeingChecked tsOrig a b where
    coerceTermWith :: tsBeingChecked -> tsOrig -> a -> b

-- we got to the end of the list of spine types, so insist on the terms being equal:
instance (SpineOf tsOrig)=> IsoTerms () tsOrig a a where
    coerceTermWith _ _ = id

instance (SpineOf (Proxy x, ts), SpineOf tsOrig
         , IsOfBaseType x a bool, TryingIsoTerm bool ts tsOrig a b
         )=> IsoTerms (Proxy x, ts) tsOrig a b where
    coerceTermWith = coerceTermWithOrContinue (Proxy :: Proxy bool) . snd

-- BASE CASE. When we've exhausted the list and have two non-equal terms we try
-- recursive type application, else type error.
instance (empty ~ (), fa ~ f a, fb ~ f b, SpineOf ts, Functor f, IsoTerms ts ts a b)=> IsoTerms empty ts fa fb where
    coerceTermWith _ ts = fmap (coerceTermWith ts ts)


-- IsoTerms helper that based on the passed type-level Bool either tries to
-- recursively coerce term a, or continues processing the type spine list by
-- calling IsoTerms again.
class TryingIsoTerm (aIsOfBaseType :: Bool) ts tsOrig a b where
    coerceTermWithOrContinue :: Proxy aIsOfBaseType -> ts -> tsOrig -> a -> b

-- a was in spine
instance (IsomorphicWith tsOrig a b)=> TryingIsoTerm True ts tsOrig a b where
    coerceTermWithOrContinue _ _ = coerceWith
-- a is not part of spine, try tail of spine list:
instance (IsoTerms ts tsOrig a b, SpineOf ts, SpineOf tsOrig)=> TryingIsoTerm False ts tsOrig a b where
    coerceTermWithOrContinue _ = coerceTermWith

