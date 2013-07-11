module Main
    where

-- We mostly care that this compiles.
--
-- Contributers: Any changes that simplify or improve the structure of the
-- Compose module are very welcome, as long as they don't break this test
-- module.

import Data.Shapely.Compose as Sh

s :: Either (Int,()) (Either (Char,()) (Bool,()))
s = Right (Right (True,()))

p :: (Int,(Char,(Bool,())))
p = (1,('a',(True,())))


-- CONCATABLE
concated_p :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
concated_p = Sh.concat (p, (p, ()))

concated_s_test = ( Sh.concat $ (Right s  :: Either (Either (Int,()) (Either (Char,()) (Bool,())))  (Either (Int,()) (Either (Char,()) (Bool,()))) ) )
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


-- Homogeneous
toList_test = Sh.toList (1,(2,(3,()))) == [1,2,3]

-- CARTESIAN-ESQUE
test_fanout = fanout (head,(tail,(length,()))) [1..3] == (1,([2,3],(3,())))

test_fanin = 
        -- the coproduct arg must be unambiguous, but hopefully in practice a
        -- type signature won't be necessary (when e.g. the sum is a
        -- TH-generated instance):
     fanin (id . fst, (const 2, (\(b,())-> if b then 3 else 0, ()))) s == 3

-- test of inferrence convenience:
replicate_test = (3  ==) $ (\(x,(y,(z,())))-> x+y+z) $ Sh.replicate 1
-- THIS DOESN'T WORK, HOWEVER. any way to restructure uncurry to make inferrence possible?
-- replicate_test2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) $ Sh.replicate 1
replicate_test2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) (Sh.replicate 1 :: (Int,(Int,(Int,()))))

extract_test = let s' :: Either (Int,()) (Either (Int,()) (Int,()))
                   s' = Right (Right (1,()))
                in extract s'  ==  (1,())
