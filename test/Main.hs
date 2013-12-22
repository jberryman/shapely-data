{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    where

-- Where a declaration starts with "prop_smoketest_" and has no type sig, it is
-- a Bool that needs to be checked; otherwise we just care that things in here
-- typecheck.
--
-- TODO make some proper quickcheck tests, especially using the polymorphic TH
-- test generator stuff.
--
-- Contributers: Any changes that simplify or improve the structure of the
-- Compose module are very welcome, as long as they don't break this test
-- module.

import Data.Shapely
import Data.Shapely.Normal as Sh
import Data.Shapely.Normal.TypeIndexed
import Data.Shapely.Spine
import Data.Proxy
import Control.Monad(forM)
import Data.Foldable(toList)
import Data.Tree
import Test.QuickCheck.All


main = do passed <- $quickCheckAll 
          if passed then putStrLn "Ok" else error "Some tests failed"


-- ---------------------------

s :: (Int,()) :+: (Char,()) :+: (Bool :*! String)
--   Either (Int,()) (Either (Char,()) (Bool,(String,())))
s = Right (Right (True,("true",())))

p :: (Int,(Char,(Bool,())))
p = 1 *: 'a' *! True
--  (1,('a',(True,())))


prop_smoketest_constructorsOfNormal_prod = constructorsOfNormal ('a',('b',())) 'x' 'y'  ==  ('x',('y',()))

{-
-- CONCATABLE
concated_p :: (Int,(Char,(Bool,(Int,(Char,(Bool,()))))))
concated_p = Sh.concat (p, (p, ()))

prop_smoketest_concated_s = ( Sh.concat $ (Right s  :: Either (Either (Int,()) (Either (Char,()) (Bool,())))  (Either (Int,()) (Either (Char,()) (Bool,(String,())))) ) )
                    == Right (Right (Right (Right (Right (True,("true",()))))))
-}

prop_smoketest_distributeTerm = 
    let s' = Right (Right (1,(True,("true",(9,())))))
     in s' == 1 *< s >* 9

multiply1
  :: Either
       (Int, (Int, ()))
       (Either
          (Int, (Char, ()))
          (Either
             (Int, (Bool, (String, ())))
             (Either
                (Char, (Int, ()))
                (Either
                   (Char, (Char, ()))
                   (Either
                      (Char, (Bool, (String, ())))
                      (Either
                         (Bool, ([Char], (Int, ())))
                         (Either
                            (Bool, ([Char], (Char, ())))
                            (Bool, ([Char], (Bool, (String, ())))))))))))
multiply1 = s >*< s

prop_smoketest_multiply2 = p >*< () >*< p == 1 *: 'a' *: True *: 1 *: 'a' *! True

prop_smoketest_multiply_monoid = and $ 
    [ () >*< p == p
    , p >*< () == p
    , () >*< s == s
    , s >*< () == s
    , (p >*< p) >*< p == p >*< (p >*< p)
    , (s >*< p) >*< p == s >*< (p >*< p)
    , (p >*< s) >*< p == p >*< (s >*< p)
    , (p >*< p) >*< s == p >*< (p >*< s)
    , (s >*< s) >*< p == s >*< (s >*< p)
    , (p >*< s) >*< s == p >*< (s >*< s)
    , (s >*< s) >*< s == s >*< (s >*< s)
    ]
    

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
prop_smoketest_fanin_prod = Sh.fanin (\i c b-> if b then (i,c) else (9,'z')) p  ==  (1,'a')
prop_smoketest_unfanin_prod = Sh.fanin (Sh.unfanin(\(i,(c,(b,())))-> if b then (i,c) else (9,'z'))) p  ==  (1,'a')

prop_smoketest_fanin_sum = 
        -- the sum arg must be unambiguous, but hopefully in practice a
        -- type signature won't be necessary (when e.g. the sum is a
        -- TH-generated instance):
        let s' = Right $ Right (1,([2..5],())) :: Either (Int,()) ( Either () (Int,([Int],())) )
         in fanin ((+1), (3, (foldr (+), ()))) s'  ==  15

prop_smoketest_unfanin_sum = 
    let f (Left (_,())) = "a"
        f (Right (Left (_,()))) = "b"
        f (Right (Right (_,(s,())))) = s
     in fanin (unfanin f) s  == "true"

-- NOTE: 'ary' is required for inference here
prop_smoketest_ary_with_unfanin = (unfanin (_4 `ary` (shiftl . Sh.reverse)) 1 2 3 4) == (3,(2,(1,(4,()))))

-- APPEND
{-
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
-}

-- Homogeneous
prop_smoketest_toList = ( toList $ toFList (1,(2,(3,()))) ) == [1,2,3]
prop_smoketest_toList2 =  null $ toList $ toFList ()  
prop_smoketest_homogenous_inferrence = (\(a,as) -> a == 1) $ fromFList $ toFList (1,(2,(3,())))

-- CARTESIAN-ESQUE
-- NOTE: ambiguous without `==`
prop_smoketest_fanout_prod = fanout (head,(tail,(Prelude.length,()))) [1..3] == (1,([2,3],(3,())))


-- test of inferrence convenience:
prop_smoketest_repeat = (3  ==) $ (\(x,(y,(z,())))-> x+y+z) $ Sh.repeat 1
-- THIS DOESN'T WORK, HOWEVER. any way to restructure fanin to make inferrence possible?
-- repeat_test2 = (3 ==) $ Sh.uncurry (\x y z-> x+y+z) $ Sh.repeat 1
prop_smoketest_repeat2 = (3 ==) $ Sh.fanin (\x y z-> x+y+z) (Sh.repeat 1 :: (Int,(Int,(Int,()))))

prop_smoketest_replicate = (\(_,(a,_)) -> a == 2) $ Sh.replicate (Proxy :: Proxy (Either () (Either () ()))) 2

prop_smoketest_extract = let s' :: Either (Int,()) (Either (Int,()) (Int,()))
                             s' = Right (Right (1,()))
                          in extract s'  ==  (1,())

prop_smoketest_factorPrefix = ('a',(True,())) == 
    (fst $ factorPrefix (Left ('a',(True,('b',()))) :: Either (Char,(Bool,(Char,()))) (Char,(Bool,())) ))

-------- MASSAGEABLE


mb = ('a',("hi",()))

mb_0 :: Either () (String,(Char,()))
mb_0 = massageNormal () 
mb_1 :: Either (String,(Char,())) ()
mb_1 = massageNormal () 
mb_2 :: Either (Int,(Char,())) (String,(Char,()))
mb_2 = massageNormal mb 
mb_3 :: Either (Char,(Int,())) (String,(Char,()))
mb_3 = massageNormal mb 
mb_4 :: Either (Char,(String,(Int,()))) (Either () (String,(Char,())))
mb_4 = massageNormal mb 
mb_5 :: Either (String,()) (String,(Char,()))
mb_5 = massageNormal mb 

mc = Left mb :: Either (Char,(String,())) ()

mc_0 :: Either (Int,()) (Either (String,(Char,())) ())
mc_0 = massageNormal mc 
    
-- Testing ordered tuples:
md = (Left ('a',('b',(3,())))) :: Either (Char,(Char,(Int,()))) ()

md_0 :: Either (Char,(Char,(Bool,()))) (Either () (Char,(Char,(Int,()))))
md_0 = massageNormal md 

prop_smoketest_md_1 = ( massageNormal md :: Either (Char,(Int,(Char,()))) (Either () (Char,(Char,(Int,())))) ) == (Right $ Right ('a',('b',(3,()))))
prop_smoketest_md_2 = ( massageNormal ('a',('b',(True,()))) :: Either (Bool,()) (Char,(Char,(Bool,()))) ) == (Right ('a',('b',(True,()))))
prop_smoketest_md_3 = ( massageNormal ('a',('b',())) :: Either (Char,(Char,())) (Either () (Int,())) ) == (Left ('a',('b',())))

{- 
-- must not typecheck
massageNormal mb :: Either (String,(Char,())) (Char,(String,())) 
massageNormal () :: Either () ()
massageNormal mc :: Either (Int,()) (Either (String,(Char,())) (String,()))
    
-- ordered product style
massageNormal md :: Either (Char,(Char,(Int,()))) (Either () (Char,(Char,(Int,()))))
massageNormal md :: Either (Char,(Char,(Bool,()))) (Either (Int,()) (Char,(Char,(Int,()))))
-}

-- Testing recursion:
prop_smoketest_mr_id = massage "foo" == "foo"

data OrderedRec = OCons Int Int OrderedRec | ONull deriving Eq
deriveShapely ''OrderedRec
prop_smoketest_orderedr_id = massage (OCons 1 1 (OCons 2 2 ONull)) == (OCons 1 1 (OCons 2 2 ONull))

-- OrderedRec but reordered constructors, plus an extra constructor to
-- demonstrate non-bijective mapping, where the cons is non-ambiguous because
-- it uses ordering-significant matching (because all terms not unique types)
data OrderedRec3 = ONull3 | OCons3 Int Int OrderedRec3 | ONewCons3 OrderedRec3 Int Int deriving Eq
deriveShapely ''OrderedRec3
prop_smoketest_rec_ordered_expand = massage (OCons 1 1 (OCons 2 2 ONull)) == (OCons3 1 1 (OCons3 2 2 ONull3))


-- [a] with both order of products and sums reversed:
data Tsil a = Snoc (Tsil a) a
          | Lin
          deriving Eq
deriveShapely ''Tsil
prop_smoketest_m_unorderedr = massage "123" == Snoc (Snoc (Snoc Lin '3') '2') '1'



-------- TYPE-INDEXED 

prop_smoketest_viewFirstTypeOf_prod =  (('a',(False,(True,("potato",())))) `viewFirstTypeOf` True) 
                              == (False,('a',(True,("potato",()))))
prop_smoketest_viewTypeOf_prod =  (('a',(False,(True,("potato",())))) `viewFirstTypeOf` "tuber") 
                        == ("potato",('a',(False,(True,()))))

viewTypeOf_sum1 :: Either (Int, ()) (Either (Char, ()) (Bool :*! String))
viewTypeOf_sum1 = s `viewTypeOf` ((1,()) :: (Int,()))

viewTypeOf_sum2 :: Either (Char, ()) (Either (Int, ()) (Bool :*! String))
viewTypeOf_sum2 = s `viewTypeOf` ('a',())

viewTypeOf_sum3 :: Either (Bool :*! String) (Either (Int, ()) (Char, ()))
viewTypeOf_sum3 = s `viewTypeOf` (True,("string",()))

prop_smoketest_viewFirstTypeOf_sum1 = (Left () :: Either () ()) `viewFirstTypeOf` ()  ==  Left ()
prop_smoketest_viewFirstTypeOf_sum2 = (Right $ Left () :: Either (Int,()) (Either () ())) `viewFirstTypeOf` ()  ==  Left ()

{- MUST NOT TYPECHECK: 
     ('a',(False,(True,("potato",())))) `viewTypeOf` True
     (Right $ Left () :: Either (Int,()) (Either () ())) `viewTypeOf` ()
     (Left () :: Either () ()) `viewTypeOf` ()
-}

nub_prod :: (Int, (Char, (Bool, ())))
nub_prod = nubType (undefined :: (Int,(Char,(Int,(Int,(Bool,(Bool,())))))))


-------- TH DERIVING:

-- NON-RECURSIVE:
data A = A deriving (Eq,Show)  -- ()
data B = B Int deriving (Eq,Show)
data C a b = C a b deriving (Eq,Show) -- (,)

data D a b = D0 a | D1 b deriving (Eq,Show) -- Either
data E a = E0 | E1 a deriving (Eq,Show) -- Maybe
data F a b c = F0 a b c | F1 a b | F2 a deriving (Eq,Show)

deriveShapely ''A
deriveShapely ''B
deriveShapely ''C
deriveShapely ''D
deriveShapely ''E
deriveShapely ''F

-- RECURSIVE: -------
data Li = Em | Co Char Li deriving Eq
deriveShapely ''Li

prop_smoketest_th_rec = 
    let a = "works" 
        b = Co 'w' $ Co 'o' $ Co 'r' $ Co 'k' $ Co 's' $ Em
     in coerce a == b && coerce b == a


data SimpleTree a = SBr (SimpleTree a) a (SimpleTree a)
                     | SEm
                     deriving (Eq,Show)
deriveShapely ''SimpleTree
data LRTree a = LRTop (LTree a) a (RTree a)
                 | LRTopEm
data LTree a = LBr (LTree a) a (RTree a)
                | LEm
data RTree a = RBr (LTree a) a (RTree a)
                | REm
fmap Prelude.concat $ forM [''LRTree , ''LTree , ''RTree ] deriveShapely

-- test deeper recursive structure: 
prop_smoketest_th_rec_multi = 
    let lrTree = LRTop (LBr LEm 'b' REm) 'a' (RBr LEm 'b' REm)
      --st0 = (Proxy :: Proxy (LRTree Char), (Proxy :: Proxy (LTree Char), (Proxy :: Proxy (RTree Char), ())))
        st0 = spine :: LRTree Char :-: LTree Char :-! RTree Char
        st1 = spine :: LRTree :-: LTree :-! RTree
     in coerceWith st0 lrTree == SBr (SBr SEm 'b' SEm) 'a' (SBr SEm 'b' SEm) &&
         coerceWith st1 lrTree == SBr (SBr SEm 'b' SEm) 'a' (SBr SEm 'b' SEm)


-- These demonstrate the need for parameter-agnostic spine elements: our type
-- is recursive, with the paramters flip-flopping. Lots of other examples.
data Simple2Tree a b = S2Br (Simple2Tree b a) a b (Simple2Tree b a)
                     | S2Em
                     deriving (Eq,Show)
deriveShapely ''Simple2Tree
data LR2Tree a b = LR2Top (L2Tree b a) a b (R2Tree b a)
                 | LR2TopEm
data L2Tree a b = L2Br (L2Tree b a) a b (R2Tree b a)
                | L2Em
data R2Tree a b = R2Br (L2Tree b a) a b (R2Tree b a)
                | R2Em
fmap Prelude.concat $ forM [''LR2Tree , ''L2Tree , ''R2Tree ] deriveShapely

-- test deeper recursive structure: 
prop_smoketest_th_rec_multi_parameter_agnostic = 
    let lrTree = LR2Top (L2Br (L2Br L2Em 'c' True R2Em) False 'b' R2Em) 'a' True (R2Br L2Em False 'b' R2Em)
        st = spine :: LR2Tree :-: L2Tree :-! R2Tree
        -- this avoids enumerating a/b, b/a variants for all types:
        -- st = spine :: LR2Tree Char Bool :-: L2Tree Char Bool :-: R2Tree Char Bool :-: 
        --                LR2Tree Bool Char :-: L2Tree Bool Char :-! R2Tree Bool Char
     in coerceWith st lrTree == S2Br (S2Br (S2Br S2Em 'c' True S2Em) False 'b' S2Em) 'a' True (S2Br S2Em False 'b' S2Em)

-- 'coerce' should handle regular recursion with parameter shuffling, because
-- it uses Unapplied:
data RegRecParams1 a b = RRPCons1 a b (RegRecParams1 b a) | RRPNil1 deriving (Eq,Show)
data RegRecParams2 a b = RRPCons2 a b (RegRecParams2 b a) | RRPNil2 deriving (Eq,Show)
fmap Prelude.concat $ forM [''RegRecParams1, ''RegRecParams2] deriveShapely

prop_smoketest_th_rec_reg_param_swapping_coerce = 
    (coerce $ RRPCons1 'a' True RRPNil1) == RRPCons2 'a' True RRPNil2

coerce_recursive_self :: [Char]
coerce_recursive_self = coerce "where the instance shows source/target term equality, and equality in outer constructors"


-- excercise coerce with recursive Functor type application
data OurTree a = OurNode a (OurForest a) deriving (Eq, Functor, Show)
data OurForest a = OurEmptyForest | OurForestCons (OurTree a) (OurForest a)
    deriving (Eq, Functor, Show) -- really a list
fmap Prelude.concat $ forM [''Tree, ''OurTree, ''OurForest] deriveShapely

ourTree = OurNode 'a' (OurForestCons (OurNode 'b' OurEmptyForest) (OurForestCons (OurNode 'c' OurEmptyForest) OurEmptyForest)) 
theirTree = Node 'a' ( [ Node 'b' [] , Node 'c' [] ]) 

prop_smoketest_coerceWith_type_application = coerceWith (spine :: OurTree :-! OurForest) ourTree  == theirTree  &&
                                   coerceWith (spine :: [] :-! Tree) theirTree == ourTree
{- TODO WE WOULD LIKE TO SUPPORT THIS:
 - where we need Shapely of OurForest to inline the newtype wrapper
data OurTree a = OurNode a (OurForest a) deriving (Functor, Show)
newtype OurForest a = OurForest [OurTree a] deriving ( Functor, Show)
fmap Prelude.concat $ forM [''Tree, ''OurTree, ''OurForest] deriveShapely

ourTree = OurNode 'a' (OurForest [OurNode 'b' (OurForest []) , OurNode 'c' (OurForest []) ])
theirTree = Node 'a' ( [ Node 'b' [] , Node 'c' [] ]) 
-}

data WithFunctorTerm1 = WFT1 (Maybe WithFunctorTerm1) (Maybe [Int]) deriving Eq
data WithFunctorTerm2 = WFT2 (Maybe WithFunctorTerm2) (Maybe [Int]) deriving Eq
fmap Prelude.concat $ forM [''WithFunctorTerm1, ''WithFunctorTerm2] deriveShapely
prop_smoketest_functor_term_sanity = coerce (WFT1 Nothing $ Just [1..3]) == (WFT2 Nothing $ Just [1..3])



-- TODO POLYMORPHISM/INFERRENCE-PRESERVING STUFF WE MIGHT LIKE TO SUPPORT SOMEHOW
-- ------------------------------
{-
prop_smoketest_th_rec_reg_param_swapping_coerce = 
    (coerce RRPNil1) == (RRPNil2 :: RegRecParams2 Char Bool)

prop_smoketest_th_rec_reg_poly_param_swapping_coerce = 
    let (x,y) = (RRPNil1,coerce x) :: (RegRecParams1 a b, RegRecParams2 a b)
     in y == RRPNil2
    -- But note: this is also (at the top level) ambiguous:
    -- foo = RRPNil2 == RRPNil2

th_rec_reg_poly_param_swapping_coerce :: (RegRecParams1 a b, RegRecParams2 a b)
th_rec_reg_poly_param_swapping_coerce = 
    let (x,y) = (RRPNil1, coerce x) 
     in (x,y)

-- if we can make FactorPrefix look like:
-- class (Product ab)=> FactorPrefix ab abcs cs | ab abcs -> cs, ab cs -> abcs, abcs cs -> ab where
-- we'd get better inferrence, supporting:
prop_smoketest_factorPrefix2 = ( ('a',(True,())) , (Left ('b',())) :: Either (Char,()) () ) == 
    (factorPrefix (Left ('a',(True,('b',())))  ))

prop_smoketest_toList2 = ( toList $ toFList () ) == []

-- currently we need: _4th `asLength` as
fanin (1,(2,(3,(4,())))) _4th


-- we'd like this type to be inferable (AGAIN TECHNICALLY POSSIBLE WITH CLOSED TYPE FAMILIES)
prop_smoketest_fanout_prod = fanout (head,(tail,(Prelude.length,()))) [1..3] == (1,([2,3],(3,())))

-}


-- ---------------------------------------------------------------------------
{-
-- TO THINK ABOUT, when doing inlining, deeper structure on next version:
-- these are old notes

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
-}
