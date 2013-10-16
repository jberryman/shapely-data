{-# LANGUAGE TypeOperators, TypeFamilies , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , UndecidableInstances #-}
module Data.Shapely.Normal.FannedApplication
    where

-- Internal module to break cycle, since we need 'Fanin' in Data.Shapely.Classes
-- We export these in Data.Shapely.Normal

import Data.Shapely.Normal.Classes

--                        TODO add comments from Fanout
-- | A class for applying a structure to a 'Product' or 'Coproduct' @t@,
-- yielding an @r@.
class Fanin t r where  -- TODO change variable names, e.g.  abcs  x
    -- | A structure capable of consuming the terms @t@ and producing @r@.
    type t :->-> r
    fanin :: (t :->-> r) -> (t -> r)

    type r :~>~> t     -- TODO change name
    fanout :: (r :~>~> t) -> (r -> t)

-- | > fanin = const
instance Fanin () r where
    type () :->-> r = r
    fanin = const

    type r :~>~> () = ()
    fanout = const  -- terminal object?

-- | n-ary @uncurry@ on 'Product's, for instance, e.g.
--
-- > fanin (+) (1,(2,())) == 3
instance (Fanin bs r)=> Fanin (a,bs) r where
    type (a,bs) :->-> r = a -> bs :->-> r
    fanin f = uncurry (fanin . f)
    
    type r :~>~> (a,bs) = (r -> a, r :~>~> bs) 
    fanout (f,fs) s = (f s, fanout fs s)
  --fanout (f,fs) = f &&& fanout fs --TODO


-- | n-ary @fanin@ on 'Coproduct's
--
-- > let s = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
-- >  in fanin ((+1), (3, (foldr (+), ()))) s  ==  15
instance (EitherTail bs, Fanin bs r, Fanin (AsTail bs) r, Fanin a r)=> Fanin (Either a bs) r where
    type Either a bs :->-> r = (a :->-> r, AsTail bs :->-> r)
    fanin (f,fs) = eitherTail (fanin f) (fanin fs)
    
    -- NOTE: no eitherTail necessary (or possible) here:
    type r :~>~> Either a bs = Either (r :~>~> a) (r :~>~> bs)
    fanout = either ((Left .) . fanout) ((Right .) . fanout) 

instance (Fanin a r)=> Fanin (Only a) r where
    type Only a :->-> r = (a :->-> r, ())
    fanin (f,()) = fanin f . just
    
    -- TODO consider hiding Only constructor, and then maybe we don't have to define this, or maybe we can make instances that are nonsensical, but work recursively
    -- NOTE: we don't even use this recursively:
    type r :~>~> Only a = r :~>~> a
    fanout f = Only . fanout f


{-
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
-}



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

instance EitherTail () where
    eitherTail f g = either f g . fmap Only
    type AsTail () = Only ()

instance EitherTail (x,y) where
    eitherTail f g = either f g . fmap Only
    type AsTail (x,y) = Only (x,y)

instance EitherTail (Either x y) where
    eitherTail = either
    type AsTail (Either x y) = Either x y


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
