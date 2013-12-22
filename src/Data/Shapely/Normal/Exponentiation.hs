{-# LANGUAGE TypeOperators, TypeFamilies , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- for nested type family application `AsTail` in `:=>->`
module Data.Shapely.Normal.Exponentiation
    where

-- Internal module to break cycle, since we need 'Exponent' in Data.Shapely.Classes
-- We export these in Data.Shapely.Normal

import Data.Shapely.Normal.Classes
import Control.Arrow((&&&))

-- | The algebraic normal form 'Exponent' @abc@ distributed over the single
-- base variable @x@.
type family abc :=>-> x
-- | The single exponent variable @x@ distributed over the algebraic normal
-- form 'Base' @abc@.
type family x :->=> abc
    
-- | x¹ = x
type instance () :=>-> x = x
-- | x⁽ᵃᵇ⁾ = (xᵇ)ᵃ
type instance (a,bs) :=>-> x = a -> bs :=>-> x
-- | x⁽ᵃ⁺ᵇ⁾ = xᵃxᵇ
type instance Either a bs :=>-> x = (a :=>-> x, AsTail bs :=>-> x)
type instance Only a :=>-> x = (a :=>-> x, ())

-- | 1ˣ = 1
type instance x :->=> () = ()
-- | (ab)ˣ = aˣbˣ
type instance x :->=> (a,bs) = (x -> a, x :->=> bs) 
-- type instance x :->=> Either a bs = 
-- type instance x :->=> Only a = 
--     This is possible but crazy; its the expansion of the multinomial (see
--     http://en.wikipedia.org/wiki/Multinomial_theorem). TODO: this, then
--     combine Exponent and Base classes.


-- TODO: consider variations with better/different type inferrence.
--       maybe adding HavingLength constraints would help


-- | A class for the exponent laws with the 'Normal' form @abc@ in the exponent
-- place. See the instance documentation for concrete types and examples.
class Exponent abc where
    fanin :: (abc :=>-> x) -> (abc -> x)
    unfanin :: (abc -> x) -> (abc :=>-> x)

-- | A class for the exponent laws with the 'Normal' form @abc@ in the base
-- place. See the instance documentation for concrete types and examples.
class Base abc where
    fanout :: (x :->=> abc) -> (x -> abc)
    unfanout :: (x -> abc) -> (x :->=> abc)

instance Exponent () where
    fanin = const
    unfanin f = f ()

instance Base () where
    fanout = const
    unfanout _ = ()

-- | [@fanin@] an n-ary @uncurry@ 
--   
--   [@unfanin@] an n-ary @curry@
--
-- Examples:
--
-- >>> fanin (+) (1,(2,()))
-- 3
-- >>> unfanin ('_4' `'ary'` ('shiftl' . 'reverse')) 1 2 3 4
-- (3,(2,(1,(4,()))))
instance (Exponent bs)=> Exponent (a,bs) where
    fanin f = uncurry (fanin . f)
    unfanin f = unfanin . curry f
    
-- | [@fanout@] an n-ary @(&&&)@
--
--   [@unfanout@] an n-ary @f :: (x -> (a,b)) -> (x -> a, x -> b)@
--
-- Examples:
--
-- >>> fanout (head,(tail,())) [1..3] == (1,([2,3],()))
-- True
instance (Base bs)=> Base (a,bs) where
    fanout (f,fs) = f &&& fanout fs
    unfanout f = (fst . f, unfanout (snd . f))
 -- unfanout = (fst .) &&& (unfanout . (snd .))

-- | [@fanin@] an n-ary @(|||)@ or 'either', (and ('Data.Shapely.Normal.!!'))
--   
--   [@unfanin@] an n-ary @f :: (Either a b -> x) -> (a -> x, b -> x)@
--
-- Examples:
--
-- >>> let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >>> fanin ((+1), (3, (foldr (+), ()))) s 
-- 15
instance (EitherTail bs, Exponent bs, Exponent (AsTail bs), Exponent a)=> Exponent (Either a bs) where
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)
    unfanin f = (unfanin (f . Left), unfanin (f . Right . fromTail))

instance (Exponent a)=> Exponent (Only a) where
    fanin (f,()) = fanin f . just
    unfanin f = (unfanin (f . Only), ())




-- ---------------------------------------------------------------------------
-- TODO move these to a Helper module (needed here and Data.Shapely.Normal
-- 
-- Our classes would be much prettier if Sums looked like (Either a
-- (Either b (Only c))), but this is not possible. All my attempts at defining
-- utility functions to simplify instances have been fairly useless, including
-- this one.

-- Helpers for recursion on sums:
class (Tail (Either () b) ~ AsTail b)=> EitherTail b where
    eitherTail :: (a -> c) -> (AsTail b -> c) -> Either a b -> c
    type AsTail b  -- keeps constraints a little less noisy
    fromTail :: AsTail b -> b

instance EitherTail () where
    eitherTail f g = either f g . fmap Only
    type AsTail () = Only ()
    fromTail = just

instance EitherTail (x,y) where
    eitherTail f g = either f g . fmap Only
    type AsTail (x,y) = Only (x,y)
    fromTail = just

instance EitherTail (Either x y) where
    eitherTail = either
    type AsTail (Either x y) = Either x y
    fromTail = id


{- NOTE initially tried this to avoid needing to define identical Sum
 - base cases for (a,b) and (), but it made instance constraint very noisey.
 - Not worth it.
-- map onto Tail of a Sum, yielding a Sum, and reconstructing. 
class (Sum b')=> FmapTail b' where
    fmapTail :: (EitherTail b)=> (Tail (Either a b) -> b') -> Either a b -> (a :< b')

instance (Sum (Either x y))=> FmapTail (Either x y) where
    fmapTail f = eitherTail Left (Right . f)

instance (Product x)=> FmapTail (Only x) where
    fmapTail f = eitherTail Left (Right . just . f)
    -}
