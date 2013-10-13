{-# LANGUAGE TypeOperators, TypeFamilies , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , UndecidableInstances #-}
module Data.Shapely.Normal.FannedApplication
    where

-- Internal module to break cycle, since we need 'Fanin' in Data.Shapely.Classes
-- We export these in Data.Shapely.Normal

import Data.Shapely.Normal.Classes



-- | A class for applying a structure to a 'Product' or 'Coproduct' @t@,
-- yielding an @r@.
class Fanin t r where
    -- | A structure capable of consuming the terms @t@ and producing @r@.
    type t :->-> r
    fanin :: (t :->-> r) -> t -> r

-- | > fanin = const
instance Fanin () r where
    type () :->-> r = r
    fanin = const

-- | n-ary @uncurry@ on 'Product's, for instance, e.g.
--
-- > fanin (+) (1,(2,())) == 3
instance (Fanin bs r)=> Fanin (a,bs) r where
    type (a,bs) :->-> r = a -> bs :->-> r
    fanin f = uncurry (fanin . f)

-- | n-ary @fanin@ on 'Coproduct's
--
-- > let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >  in fanin ((+1), (3, (foldr (+), ()))) s  ==  15
instance (Fanin (Tail (Either a bs)) r, EitherTail bs, Fanin a r)=> Fanin (Either a bs) r where
    type Either a bs :->-> r = (a :->-> r, Tail (Either a bs) :->-> r)
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)

instance (Fanin a r)=> Fanin (Only a) r where
    type Only a :->-> r = (a :->-> r, ())
    fanin (f,()) = fanin f . just



-- | Apply a 'Product' or 'Coproduct' structure @fs@ to the term @s@ yielding a
-- product or coproduct @FannedOut fs@.
--
-- See also 'Fanin'.
class Fanout s fs | fs -> s where
    type FannedOut fs
    fanout :: fs -> (s -> FannedOut fs)

instance Fanout s () where
    type FannedOut () = ()
    fanout = const

-- ...and this is the result of @uncurry@ with arrows reversed.
-- | an n-ary @(&&&)@
instance (Fanout s fs)=> Fanout s (s -> x,fs) where
    type FannedOut (s -> x, fs) = (x, FannedOut fs) 
    fanout (f,fs) s = (f s, fanout fs s)

-- @fs@ is what we need to get @r -> E a b@, which is the result of @fanin@
-- with the arrow reversed. I'm not sure what to call this.
-- | e.g. 
--
-- > fanout (Left ((+1),()) :: Either (Int -> Int,()) (Int -> Bool,())) 1  ==  Left (2,())
instance (Fanout s fs, Fanout s fss)=> Fanout s (Either fs fss) where
    type FannedOut (Either fs fss) = Either (FannedOut fs) (FannedOut fss)
    fanout = either ((Left .) . fanout) ((Right .) . fanout) -- NOTE eitherTail/Only instance unnecessary




-- ---------------------------------------------------------------------------
-- TODO move these to a Helper module (needed here and Data.Shapely.Normal

-- Helpers for recursion on coproducts:
class EitherTail b where
    eitherTail :: (a -> c) -> (Tail (Either a b) -> c) -> Either a b -> c

instance EitherTail () where
    eitherTail f g = either f g . fmap Only

instance EitherTail (x,y) where
    eitherTail f g = either f g . fmap Only

instance EitherTail (Either x y) where
    eitherTail = either

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
