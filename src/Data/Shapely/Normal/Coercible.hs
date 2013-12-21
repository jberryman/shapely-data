{-# LANGUAGE MultiParamTypeClasses
 , TypeFamilies, FlexibleInstances, FlexibleContexts , FunctionalDependencies
 , OverlappingInstances -- for CoercibleNormalWith to support recursive subterms
 , UndecidableInstances
 , ScopedTypeVariables -- for bool passing trick in 2nd CoerceTerms instance
 , PolyKinds, DataKinds
 #-}
module Data.Shapely.Normal.Coercible
    where

-- separated, again, to contain OverlappingInstances

import Data.Shapely.Category
import Data.Shapely.Classes
import Data.Shapely.Utilities

import Data.Shapely.Spine
import Data.Proxy.Kindness


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
instance (Unapplied (Proxy txy) t, CoercibleWith (Proxy t,()) txy b)=> Isomorphic txy b where
    coerce a = coerceWith (unappliedOf a, ()) a


-- | A 'Shapely' type @a@ coercible to @b@ where types in the spine @ts@ are
-- recursively 'coerceWith'-ed to @b@.
class (SpineOf ts, Shapely a, Shapely b)=> CoercibleWith ts a b where
    -- | Convert a type @a@ to an isomorphic type @b@, where the 'Proxy' types
    -- in @ts@ define the recursive structure of @a@. See 'SpineOf'. 
    --
    -- These terms will be converted to the target type when they appear as
    -- top-level product terms or in nested 'Functor' applications.
    coerceWith :: ts -> a -> b

instance (Shapely a, Shapely b, CoercibleNormalWith ts (Normal a) (Normal b))=> CoercibleWith ts a b where
    coerceWith ts a = coerceNormalWith ts $$ a


-- ---------------- INTERNAL (for now)

class (SpineOf ts)=> CoercibleNormalWith ts na nb where
    coerceNormalWith :: ts -> na -> nb

instance (SpineOf ts)=> CoercibleNormalWith ts () () where
    coerceNormalWith _ = id

instance (CoerceTerms ts ts x y, CoercibleNormalWith ts as bs)=> CoercibleNormalWith ts (x,as) (y,bs) where
    coerceNormalWith ts (x,as) = (coerceTermWith ts ts x, coerceNormalWith ts as)

instance (CoercibleNormalWith a as as', CoercibleNormalWith a bs bs')=> CoercibleNormalWith a (Either as bs) (Either as' bs') where
    coerceNormalWith a' = bimap (coerceNormalWith a') (coerceNormalWith a')



class (SpineOf tsOrig, SpineOf tsBeingChecked)=> CoerceTerms tsBeingChecked tsOrig a b where
    coerceTermWith :: tsBeingChecked -> tsOrig -> a -> b

-- we got to the end of the list of spine types, so insist on the terms being equal:
instance (SpineOf tsOrig)=> CoerceTerms () tsOrig a a where
    coerceTermWith _ _ = id

instance (SpineOf (Proxy x, ts), SpineOf tsOrig
         , IsOfBaseType x a bool, TryingCoerceTerm bool ts tsOrig a b
         )=> CoerceTerms (Proxy x, ts) tsOrig a b where
    coerceTermWith = coerceTermWithOrContinue (Proxy :: Proxy bool) . snd

-- BASE CASE. When we've exhausted the list and have two non-equal terms we try
-- recursive type application, else type error.
instance (empty ~ (), fa ~ f a, fb ~ f b, SpineOf ts, Functor f, CoerceTerms ts ts a b)=> CoerceTerms empty ts fa fb where
    coerceTermWith _ ts = fmap (coerceTermWith ts ts)


-- CoerceTerms helper that based on the passed type-level Bool either tries to
-- recursively coerce term a, or continues processing the type spine list by
-- calling CoerceTerms again.
class TryingCoerceTerm (aIsOfBaseType :: Bool) ts tsOrig a b where
    coerceTermWithOrContinue :: Proxy aIsOfBaseType -> ts -> tsOrig -> a -> b

-- a was in spine
instance (CoercibleWith tsOrig a b)=> TryingCoerceTerm True ts tsOrig a b where
    coerceTermWithOrContinue _ _ = coerceWith
-- a is not part of spine, try tail of spine list:
instance (CoerceTerms ts tsOrig a b, SpineOf ts, SpineOf tsOrig)=> TryingCoerceTerm False ts tsOrig a b where
    coerceTermWithOrContinue _ = coerceTermWith

