{-# LANGUAGE TypeOperators, TypeFamilies , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , UndecidableInstances #-}
module Data.Shapely.Normal.FannedApplication
    where

-- Internal module to break cycle, since we need 'Fans' in Data.Shapely.Classes
-- We export these in Data.Shapely.Normal

import Data.Shapely.Normal.Classes
import Control.Arrow((&&&))

-- TODO: leave `r` out of instance head and move type funcs out?

-- | A class for arrows between a 'Product' or 'Coproduct' @abc@ and any type @r@. 
class Fans abc r where
    -- | A structure capable of consuming the terms @abc@ and producing @r@.
    type abc :=>-> r
    fanin :: (abc :=>-> r) -> (abc -> r)
    unfanin :: (abc -> r) -> (abc :=>-> r)

    -- | A structure capable of producing the terms @abc@ from @r@
    type r :->=> abc
    fanout :: (r :->=> abc) -> (r -> abc)

instance Fans () r where
    type () :=>-> r = r
    fanin = const
    unfanin f = f ()

    type r :->=> () = ()
    fanout = const

-- | [@fanin@] an n-ary @uncurry@ 
--   
--   [@unfanin@] an n-ary @curry@
--
--   [@fanout@] an n-ary @(&&&)@
--
-- Examples:
--
-- > fanin (+) (1,(2,())) == 3
instance (Fans bs r)=> Fans (a,bs) r where
    type (a,bs) :=>-> r = a -> bs :=>-> r
    fanin f = uncurry (fanin . f)
    unfanin f = unfanin . curry f
    
    type r :->=> (a,bs) = (r -> a, r :->=> bs) 
    fanout (f,fs) = f &&& fanout fs

-- | [@fanin@] an n-ary @(|||)@
--   
--   [@unfanin@] an n-ary ???
--
--   [@fanout@] an n-ary ???
--
-- Examples:
--
-- > let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >  in fanin ((+1), (3, (foldr (+), ()))) s  ==  15
--
-- > fanout (Left ((+1),()) :: Either (Int -> Int,()) (Int -> Bool,())) 1  ==  Left (2,())
instance (EitherTail bs, Fans bs r, Fans (AsTail bs) r, Fans a r)=> Fans (Either a bs) r where
    type Either a bs :=>-> r = (a :=>-> r, AsTail bs :=>-> r)
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)
    unfanin f = (unfanin (f . Left), unfanin (f . Right . fromTail))
    
    -- NOTE: no eitherTail necessary (or possible) here:
    type r :->=> Either a bs = Either (r :->=> a) (r :->=> bs)
    fanout = either ((Left .) . fanout) ((Right .) . fanout) 

instance (Fans a r)=> Fans (Only a) r where
    type Only a :=>-> r = (a :=>-> r, ())
    fanin (f,()) = fanin f . just
    unfanin f = (unfanin (f . Only), ())
    
    -- TODO consider hiding Only constructor, and then maybe we don't have to define this, or maybe we can make instances that are nonsensical, but work recursively
    -- NOTE: we don't even use this recursively:
    type r :->=> Only a = r :->=> a
    fanout f = Only . fanout f





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
