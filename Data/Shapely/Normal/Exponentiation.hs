{-# LANGUAGE TypeOperators, TypeFamilies , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , UndecidableInstances #-}
module Data.Shapely.Normal.Exponentiation
    where

-- Internal module to break cycle, since we need 'Exponent' in Data.Shapely.Classes
-- We export these in Data.Shapely.Normal

import Data.Shapely.Normal.Classes
import Control.Arrow((&&&))

-- | The algebraic normal form exponent @abc@ distributed over the base @r@.
type family abc :=>-> r
-- | The exponent @r@ distributed over the algebraic normal form @abc@.
type family r :->=> abc
    
-- TODO document with nice math symbols
type instance () :=>-> r = r
type instance (a,bs) :=>-> r = a -> bs :=>-> r
type instance Either a bs :=>-> r = (a :=>-> r, AsTail bs :=>-> r)
type instance Only a :=>-> r = (a :=>-> r, ())

type instance r :->=> () = ()
type instance r :->=> (a,bs) = (r -> a, r :->=> bs) 
-- type instance r :->=> Either a bs = 
-- type instance r :->=> Only a = 
--     This is possible but crazy; its the expansion of the multinomial (see
--     http://en.wikipedia.org/wiki/Multinomial_theorem). TODO: this, then
--     combine Exponent and Base classes.



-- | A class for the exponent laws with the 'Normal' form @abc@ in the exponent
-- place. See the instance documentation for concrete types and examples.
class Exponent abc where
    fanin :: (abc :=>-> r) -> (abc -> r)
    unfanin :: (abc -> r) -> (abc :=>-> r)

-- | A class for the exponent laws with the 'Normal' form @abc@ in the base
-- place. See the instance documentation for concrete types and examples.
class Base abc where
    fanout :: (r :->=> abc) -> (r -> abc)
    unfanout :: (r -> abc) -> (r :->=> abc)

instance Exponent () where
    fanin = const
    unfanin f = f ()

instance Base () where
    fanout = const
    unfanout f = ()

-- | [@fanin@] an n-ary @uncurry@ 
--   
--   [@unfanin@] an n-ary @curry@
--
-- Examples:
--
-- >>> fanin (+) (1,(2,()))
-- 3
instance (Exponent bs)=> Exponent (a,bs) where
    fanin f = uncurry (fanin . f)
    unfanin f = unfanin . curry f
    
-- | [@fanout@] an n-ary @(&&&)@
--
instance (Base bs)=> Base (a,bs) where
    fanout (f,fs) = f &&& fanout fs
    unfanout f = (fst . f, unfanout (snd . f))
 -- unfanout = (fst .) &&& (unfanout . (snd .))

-- | [@fanin@] an n-ary @(|||)@, and (!!)
--   
--   [@unfanin@] an n-ary TODO
--
-- Examples:
--
-- >>> let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >>> fanin ((+1), (3, (foldr (+), ()))) s 
-- 15
--
-- And for fetching an element at an index...
--
-- >>> fanin (1,(2,(3,(4,())))) _4th
-- 4
instance (EitherTail bs, Exponent bs, Exponent (AsTail bs), Exponent a)=> Exponent (Either a bs) where
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)
    unfanin f = (unfanin (f . Left), unfanin (f . Right . fromTail))

instance (Exponent a)=> Exponent (Only a) where
    fanin (f,()) = fanin f . just
    unfanin f = (unfanin (f . Only), ())




-- ---------------------------------------------------------------------------
-- TODO move these to a Helper module (needed here and Data.Shapely.Normal
-- 
-- Our classes would be much prettier if Coproducts looked like (Either a
-- (Either b (Only c))), but this is not possible. All my attempts at defining
-- utility functions to simplify instances have been fairly useless, including
-- this one.

-- Helpers for recursion on coproducts:
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


{- NOTE initially tried this to avoid needing to define identical Coproduct
 - base cases for (a,b) and (), but it made instance constraint very noisey.
 - Not worth it.
-- map onto Tail of a Coproduct, yielding a Coproduct, and reconstructing. 
class (Coproduct b')=> FmapTail b' where
    fmapTail :: (EitherTail b)=> (Tail (Either a b) -> b') -> Either a b -> (a :< b')

instance (Coproduct (Either x y))=> FmapTail (Either x y) where
    fmapTail f = eitherTail Left (Right . f)

instance (Product x)=> FmapTail (Only x) where
    fmapTail f = eitherTail Left (Right . just . f)
    -}
