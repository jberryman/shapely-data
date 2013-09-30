{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Main
    where

-- We mostly care that this compiles.
--
-- Contributers: Any changes that simplify or improve the structure of the
-- Compose module are very welcome, as long as they don't break this test
-- module.

import Data.Shapely
import Data.Shapely.Compose as Sh

s :: (Int,()) :+: (Char,()) :+: (Bool,())
--   Either (Int,()) (Either (Char,()) (Bool,()))
s = Right (Right (True,()))

p :: (Int,(Char,(Bool,())))
p = 1 <| 'a' <! True
--  (1,('a',(True,())))


-- CONCATABLE
concated_p :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
concated_p = Sh.concat (p, (p, ()))

test_concated_s = ( Sh.concat $ (Right s  :: Either (Either (Int,()) (Either (Char,()) (Bool,())))  (Either (Int,()) (Either (Char,()) (Bool,()))) ) )
                    == Right (Right (Right (Right (Right (True,())))))


-- REVERSABLE
s_rev :: Either (Bool,()) (Either (Char,()) (Int,()))
s_rev = Sh.reverse s

p_rev :: (Bool,(Char,(Int,())))
p_rev = Sh.reverse p

p_empty_rev :: ()
p_empty_rev = Sh.reverse ()

-- SHIFTING:
sr :: Either (Bool,()) (Either (Int,()) (Char,()))
sr = shiftr s

sl :: Either (Char,()) (Either (Bool,()) (Int,()))
sl = shiftl s

pr :: (Bool,(Int,(Char,())))
pr = shiftr p

pl :: (Char,(Bool,(Int,())))
pl = shiftl p

-- UNCURRY
test_uncurry = Sh.uncurry (\i c b-> if b then (i,c) else (9,'z')) p  ==  (1,'a')

-- APPEND
appended :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
appended = p .++. p

appended_s
  :: Either
       (Char, ())
       (Either
          (Int, ()) (Either (Int, ()) (Either (Char, ()) (Bool, ()))))
appended_s = let s_ss = (Right s) :: Either ( Either (Char,()) (Int,()) )  ( Either (Int,()) (Either (Char,()) (Bool,())) )
              in append s_ss
                  --  == Right (Right (Right (Right (True,()))))

-- Homogeneous
test_toList = Sh.toList (1,(2,(3,()))) == [1,2,3]

-- CARTESIAN-ESQUE
test_fanout = fanout (head,(tail,(length,()))) [1..3] == (1,([2,3],(3,())))

test_fanin = 
        -- the coproduct arg must be unambiguous, but hopefully in practice a
        -- type signature won't be necessary (when e.g. the sum is a
        -- TH-generated instance):
    let s' :: Either (Int,()) (Either () ([Int],([Int],())))
        s' = Left (1,()) 
     in fanin ((+1), (3, ((length .) . (++), ()))) s' == 2

-- test of inferrence convenience:
test_replicate = (3  ==) $ (\(x,(y,(z,())))-> x+y+z) $ Sh.replicate 1
-- THIS DOESN'T WORK, HOWEVER. any way to restructure uncurry to make inferrence possible?
-- replicate_test2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) $ Sh.replicate 1
test_replicate2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) (Sh.replicate 1 :: (Int,(Int,(Int,()))))

test_extract = let s' :: Either (Int,()) (Either (Int,()) (Int,()))
                   s' = Right (Right (1,()))
                in extract s'  ==  (1,())


-- Massageable tests:
{- 
*Data.Shapely.Compose.Massageable> let b = ('a',("hi",()))

    -- must not typecheck
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,(Char,())) (Char,(String,())) 
    *Data.Shapely.Compose.Massageable> massageNormal () :: Either () ()

    -- must typecheck:
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (Int,(Char,())) (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal () :: Either () (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal () :: Either (String,(Char,())) ()
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (Char,(Int,())) (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (Char,(String,(Int,()))) (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,()) (String,(Char,()))

*Data.Shapely.Compose.Massageable Control.Arrow> let c = (Left ('a',("hi",()))) :: Either (Char,(String,())) ()

    *Data.Shapely.Compose.Massageable Control.Arrow> massageNormal c :: Either (Int,()) (Either (String,(Char,())) ())
    
Testing recursion:


    -- [a] with both order of products and coproducts reversed:
    data Tsil a = Snoc (Tsil a) a
              | Lin
              deriving Show
    instance Shapely (Tsil a) where
        type Normal (Tsil a) = Either (AlsoNormal (Tsil a), (a, ())) ()
        toNorm (Snoc l n) = Left (Also . toNorm $ l , (n ,()))
        toNorm Lin = Right ()
        fromNorm (Right ()) = Lin
        fromNorm (Left (an,(n,()))) = Snoc (fromNorm . normal $ an) n

    massageRec "123"  :: Tsil Char

 -}
