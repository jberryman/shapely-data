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
import Data.Shapely.Normal as Sh
import Data.Shapely.Normal.TypeIndexed

--  TODO QUESTIONS:
    -- How does this work with polymorphic terms?
    -- And what about in Massageable, can we do massage (:: Foo a b) :: Bar b a   ??
    -- What about the fact that users can instantiate a polymorphic term as AlsoNormal, what happens there?
    -- can we use the "extra method trick"?
    -- what about types like data `Fix f = f (Fix f)`


s :: (Int,()) :+: (Char,()) :+: (Bool :*! String)
--   Either (Int,()) (Either (Char,()) (Bool,(String,())))
s = Right (Right (True,("true",())))

p :: (Int,(Char,(Bool,())))
p = 1 <| 'a' <! True
--  (1,('a',(True,())))


test_constructorsOfNormal_prod = constructorsOfNormal ('a',('b',())) 'x' 'y'  ==  ('x',('y',()))

-- CONCATABLE
concated_p :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
concated_p = Sh.concat (p, (p, ()))

test_concated_s = ( Sh.concat $ (Right s  :: Either (Either (Int,()) (Either (Char,()) (Bool,())))  (Either (Int,()) (Either (Char,()) (Bool,(String,())))) ) )
                    == Right (Right (Right (Right (Right (True,("true",()))))))


-- REVERSABLE
s_rev :: Either (Bool,(String,())) (Either (Char,()) (Int,()))
s_rev = Sh.reverse s

p_rev :: (Bool,(Char,(Int,())))
p_rev = Sh.reverse p

p_empty_rev :: ()
p_empty_rev = Sh.reverse ()

-- SHIFTING:
sr :: Either (Bool,(String,())) (Either (Int,()) (Char,()))
sr = shiftr s

sl :: Either (Char,()) (Either (Bool,(String,())) (Int,()))
sl = shiftl s

pr :: (Bool,(Int,(Char,())))
pr = shiftr p

pl :: (Char,(Bool,(Int,())))
pl = shiftl p

-- FANIN
test_fanin_prod = Sh.fanin (\i c b-> if b then (i,c) else (9,'z')) p  ==  (1,'a')
test_unfanin_prod = Sh.fanin (Sh.unfanin(\(i,(c,(b,())))-> if b then (i,c) else (9,'z'))) p  ==  (1,'a')

test_fanin_coprod = 
        -- the coproduct arg must be unambiguous, but hopefully in practice a
        -- type signature won't be necessary (when e.g. the sum is a
        -- TH-generated instance):
        let s' = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
         in fanin ((+1), (3, (foldr (+), ()))) s'  ==  15

test_unfanin_coprod = 
    let f (Left (_,())) = "a"
        f (Right (Left (_,()))) = "b"
        f (Right (Right (_,(s,())))) = s
     in fanin (unfanin f) s  == "true"


-- APPEND
appended :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
appended = p .++. p

appended_s
  :: Either
       (Char, ())
       (Either
          (Int, ()) (Either (Int, ()) (Either (Char, ()) (Bool, (String,())))))
appended_s = let s_ss = (Right s) :: Either ( Either (Char,()) (Int,()) )  ( Either (Int,()) (Either (Char,()) (Bool,(String,()))) )
              in append s_ss
                  --  == Right (Right (Right (Right (True,()))))

-- Homogeneous
test_toList = Sh.toList (1,(2,(3,()))) == [1,2,3]

-- CARTESIAN-ESQUE
test_fanout_prod = fanout (head,(tail,(length,()))) [1..3] == (1,([2,3],(3,())))

test_fanout_coprod = fanout (Right $ Left ((+1),(const 'a',())) ) 1  ==  (Right (Left (2,('a',()))) :: Either (Bool,()) (Either (Int,(Char,()))  ()))

-- test of inferrence convenience:
test_replicate = (3  ==) $ (\(x,(y,(z,())))-> x+y+z) $ Sh.replicate 1
-- THIS DOESN'T WORK, HOWEVER. any way to restructure fanin to make inferrence possible?
-- replicate_test2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) $ Sh.replicate 1
test_replicate2 = (3 ==) $ Sh.fanin (\x y z-> x+y+z) (Sh.replicate 1 :: (Int,(Int,(Int,()))))

test_extract = let s' :: Either (Int,()) (Either (Int,()) (Int,()))
                   s' = Right (Right (1,()))
                in extract s'  ==  (1,())


-- Massageable tests:
--
-- TODO check recursion with target having multiple *different* but equivalent
-- *massageable* subterms. (or something)
-- TODO test how TIP class works with e.g. (AlsoNormal a, (b, (AlsoNormal
-- c,()))), where AlsoNormal a ~ AlsoNormal c (and where it does not). Are
-- these considered equal, and so product would be order-significant? Should we
-- say that all AlsoNormal subterms are equal for this purpose?
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
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (Char,(String,(Int,()))) (Either () (String,(Char,())))
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,()) (String,(Char,()))

*Data.Shapely.Compose.Massageable Control.Arrow> let c = (Left ('a',("hi",()))) :: Either (Char,(String,())) ()

    *Data.Shapely.Compose.Massageable Control.Arrow> massageNormal c :: Either (Int,()) (Either (String,(Char,())) ())
    
    -- must not typecheck:
    massageNormal c :: Either (Int,()) (Either (String,(Char,())) (String,()))

-- Testing ordered tuples:
*Data.Shapely.Compose.Massageable Control.Arrow> let d = (Left ('a',('b',(3,())))) :: Either (Char,(Char,(Int,()))) ()

    -- must not typecheck
    massageNormal d :: Either (Char,(Char,(Int,()))) (Either () (Char,(Char,(Int,()))))
    massageNormal d :: Either (Char,(Char,(Bool,()))) (Either (Int,()) (Char,(Char,(Int,()))))

    -- must typecheck:
    massageNormal d :: Either (Char,(Char,(Bool,()))) (Either () (Char,(Char,(Int,()))))
    massageNormal d :: Either (Char,(Int,(Char,()))) (Either () (Char,(Char,(Int,())))) == Right $ Right ('a',('b',(3,())))
    massageNormal ('a',('b',(True,()))) :: Either (Bool,()) (Char,(Char,(Bool,())))
    massageNormal ('a',('b',())) :: Either (Char,(Char,())) (Either () (Int,()))

    
-- Testing recursion:


    -- [a] with both order of products and coproducts reversed:
    data Tsil a = Snoc (Tsil a) a
              | Lin
              deriving Show

    instance Shapely (Tsil a) where
        type Normal (Tsil a) = Either (AlsoNormal (Tsil a), (a, ())) ()
        to (Snoc l n) = Left (Also . to $ l , (n ,()))
        to Lin = Right ()
        from (Right ()) = Lin
        from (Left (an,(n,()))) = Snoc (from . normal $ an) n
        
        constructorsOf _ = (\an n-> Snoc (from . normal $ an) n, (Lin,()))
      --constructorsOf _ = (Snoc,(Lin,()))  -- recursive!
        or :
        from = fanin constructors . nonrecursive

    massageRec "123"  :: Tsil Char

 -}


-------- TYPE-INDEXED 

test_viewFirstTypeOf_prod =  (('a',(False,(True,("potato",())))) `viewFirstTypeOf` True) 
                              == (False,('a',(True,("potato",()))))
test_viewTypeOf_prod =  (('a',(False,(True,("potato",())))) `viewFirstTypeOf` "tuber") 
                        == ("potato",('a',(False,(True,()))))

viewTypeOf_coprod1 :: Either (Int, ()) (Either (Char, ()) (Bool :*! String))
viewTypeOf_coprod1 = s `viewTypeOf` ((1,()) :: (Int,()))

viewTypeOf_coprod2 :: Either (Char, ()) (Either (Int, ()) (Bool :*! String))
viewTypeOf_coprod2 = s `viewTypeOf` ('a',())

viewTypeOf_coprod3 :: Either (Bool :*! String) (Either (Int, ()) (Char, ()))
viewTypeOf_coprod3 = s `viewTypeOf` (True,("string",()))

test_viewFirstTypeOf_coprod1 = (Left () :: Either () ()) `viewFirstTypeOf` ()  ==  Left ()
test_viewFirstTypeOf_coprod2 = (Right $ Left () :: Either (Int,()) (Either () ())) `viewFirstTypeOf` ()  ==  Left ()

{- MUST NOT TYPECHECK: 
     ('a',(False,(True,("potato",())))) `viewTypeOf` True
     (Right $ Left () :: Either (Int,()) (Either () ())) `viewTypeOf` ()
     (Left () :: Either () ()) `viewTypeOf` ()
-}

nub_prod :: (Int, (Char, (Bool, ())))
nub_prod = nubType (undefined :: (Int,(Char,(Int,(Int,(Bool,(Bool,())))))))


-------- TH DERIVING:

data A = A deriving (Eq,Show)  -- ()
data B = B Int deriving (Eq,Show)
data C a b = C a b deriving (Eq,Show) -- (,)

data D a b = D0 a | D1 b deriving (Eq,Show) -- Either
data E a = E0 | E1 a deriving (Eq,Show) -- Maybe
data F a b c = F0 a b c | F1 a b | F2 a deriving (Eq,Show)

deriveShapely [''A]
deriveShapely [''B]
deriveShapely [''C]
deriveShapely [''D]
deriveShapely [''E]
deriveShapely [''F]

newtype Strange0 a = Strange0 (Either a (Strange0 a))
-- must pass `Strange0` as recursive target.
newtype Strange1 = Strange1 [Strange1]
-- e.g. (S1 []) : (S1 [ S1 [], S1 [] ]) : []
-- Either () (AlsoNormal Strange1, (AlsoNormal [Strange1], ()))
-- we take normal form from argument [Strange1]:
--     Either () (Strange1,([Strange1],()))
-- ...but pass along *both* the newtype and inner wrapped type as recursion candidates
data OddTree a rt = OddBranch (OddTree a rt) a rt | OddLeaf
newtype Strange3 a = Strange3 (OddTree a (Strange3 a))
-- Either (AlsoNormal (OddTree a (Strange3 a)), (a, (AlsoNormal (Strange3 a), ()))) ()
-- (this is the same as Strange1)
newtype Strange4 = Strange4 ([Either Strange4 Int]) -- a strange rose tree
-- we have a mutually-recursive structure, but where recursive subterms are not at top-level, same as:
data Strange4' = Cons4' (Either Strange4' Int) Strange4' | Empty4'
-- Either (Either (AlsoNormal (Strange4')) (Int,()) , (AlsoNormal Strange4', ())) ()
--         \ ____________________________________ /
--              Normal (Either Strange4' Int)
--
-- We can't wrap in AlsoNormal, because an instance AlsoNormal (Either
-- Strange4' Int) would overlap . But if that Either was a type we didn't have
-- a Shapely instance for, we'd need to generate it. But we'd in turn need to
-- generate the instance for the newtype-wrapped type, since we need its
-- recursive Strange4' term bound. So...
--
-- A different approach seems in order:
--    - reify all *exposed* types on the RHS of type declaration
--    - add AlsoNormal wrappers everywhere necessary to break cycles
--        this might mean doing AlsoNormal [Foo] but keeping [Bar]
-- 
-- Or maybe transform to a "flat" type first? by running an `mconcat`, e.g.
-- Strange4' becomes:
--     data Strange4' = Cons4'A Strange4' Strange4' 
--                    | Cons4'B Int Strange4' 
--                    | Empty4'
-- And `data Bar a= Bar Int ((a,Char) , Int)` becomes:
--     data Bar a = Bar Int a Char Int
--
-- But then how de we differentiate between an Int term (which we shouldn't try
-- to "unpack") and a Foo term? Just if it has arguments or not? 
--
-- Perhaps look at other generics libraries and see what they do.
