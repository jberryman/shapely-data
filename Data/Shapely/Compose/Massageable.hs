{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}  -- for "advanced overlap" solution
module Data.Shapely.Compose.Massageable
    where

import Data.Shapely.Classes
import Data.Shapely.Compose.Classes

-- An internal module mostly to keep use of OverlappingInstances isolated


-- We borrow this type-equality comparison trick from Oleg: 
--   http://okmij.org/ftp/Haskell/ConEQ.hs
data Yes
data No

class HasAny a l b | a l -> b

instance HasAny a (a,l) Yes
instance (HasAny a l b)=> HasAny a (x,l) b
instance HasAny a () No

-- for 'Coproduct's:
instance HasAny p (Either p ps) Yes
instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
instance HasAny p (Only p) Yes
instance (b ~ No)=> HasAny p (Only x) b

-- | The non-empty, type-indexed product @l@, out of which we can pull the unique type @a@, leaving @l'@
class TIP a l l' | a l -> l' where
    viewType :: l -> (a,l')

instance (HasAny a l No)=> TIP a (a,l) l where
    viewType = id

instance (TIP a l l', (x,l') ~ xl')=> TIP a (x,l) xl' where
  --viewType = swapFront . fmap viewType
    viewType (x,l) = let (a,l') = viewType l
                      in (a,(x,l'))


-- TODO variations (perhaps with type-level flags) for:
--          - non-shuffling of product terms when mapping to a coproduct
--          - ignore recursive terms

-- | A class for typed, hopefully-principled, \"fuzzy coercions\" between types
-- in 'Normal' form. This works as follows (or see examples below):
--
--   - when the target type @t@ is a 'Product', the terms of the source
--     product(s) are re-arranged to match the target. This must be an
--     unambiguous bijection to typecheck, so all types in the target must be
--     unique i.e. a \"type-indexed product\" (TIP)
--
--   - when @t@ is a 'Coproduct', the ordering of product terms in @s@ (whether
--     @s@ is a 'Product' or sum of products) is retained, i.e. is considered
--     to be significant, and no shuffling of these terms is done.
--
--   - When the source @s@ is a 'Coproduct' this conversion may be surjective,
--     i.e. multiple input \"constructors\" may map to the same output coproduct
--     position, but this mapping has to be unambiguous.
--
-- This behavior is partially out of necessity, but I think makes sense if you
-- believe the following:
--
--   - records aren't appropriate for sum types since they're partial
--   - if you're using record type you should use records as your only interface to the type
--
-- ...and you want this function to never do something you're not expecting.
-- I've tried to strike a good balance and accomodate both styles of treating
-- data types.
--
-- Here are some examples:
--
--    TODO put code we used in tests here !!!
--
class Massageable s t where
    massageNormal :: s -> t

instance Massageable () () where
    massageNormal = id

instance (Massageable s' l, TIP a (x,y) s')=> Massageable (x,y) (a,l) where
    massageNormal = fmap massageNormal . viewType

-- this will treat source as sum of TIPs when target is a 'Product', and drop
-- to the MassageableToCoproduct instance if a 'Coproduct'
instance (Massageable s t, Massageable ss t)=> Massageable (Either s ss) t where
    massageNormal = either massageNormal massageNormal

instance (MassageableToCoproduct (x,y) (Either t ts))=> Massageable (x,y) (Either t ts) where
    -- Drop into a 'massage' that observers product ordering, for when we
    -- hit the base case (x,y) (x',y'):
    massageNormal = massageNormalToCoproduct

instance (MassageableToCoproduct () (Either t ts))=> Massageable () (Either t ts) where
    massageNormal = massageNormalToCoproduct

-------
-- When going product -> coproduct we need to be able to choose an instance
-- based on *whether* the source product and left are massageable or not. In
-- order to do that we have to turn all our classes above into *predicates*,
-- i.e. where no instance would exist above, we now need to define an instance
-- that unifies a boolean head type variable to 'No'; we then chain these using
-- boolean algebra in our constraints.
--
-- The point is, I'm sorry...
--
-- technique adapted from: 
--     http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap


-- Predicate for determining if MassageableToCoproduct' should map to the Left
-- or recurse to the Right of a Coproduct
class LeftMassageable s t b | s t -> b
instance (LeftMassageable xxs as b)=> LeftMassageable xxs (Either as bs) b
-- ...and we reuse this for the product -> product predicate as well:
instance LeftMassageable () () Yes
instance (No ~ no)=> LeftMassageable (x,y) () no
instance (No ~ no)=> LeftMassageable () (x,y) no
instance (And b0 b1 b, LeftMassageable s' l b0, TIPable a (x,y) s' b1)=> LeftMassageable (x,y) (a,l) b

-- TODO This isn't really what we want; see below*
class TIPable a l l' b | a l -> l', a l l' -> b
-- Yes, but only if tail has no 'a':
instance (HasAny a l b0, Not b0 b)=> TIPable a (a,l) l b
-- recurse, looking in tail for 'a':
instance (TIPable a l l' b, (x,l') ~ xl')=> TIPable a (x,l) xl' b
instance (No ~ no, No ~ none)=> TIPable a () none no
-- *our TIPable class doesn't really work as a predicate, since the decomposition
-- (whose existence we want to express in 'b') is in the instance head, so
-- in our fall-through instance above we replace where the list remainder
-- should be with 'No' and catch it in the instances below. Sorry...
instance (No ~ no)=> LeftMassageable No () no
instance (No ~ no)=> LeftMassageable No (x,y) no
instance (No ~ no)=> TIPable a No x no
instance (No ~ no)=> HasAny a No no


class And a b c | a b -> c
instance And Yes b b
instance And No  b No

class Or a b c | a b -> c
instance Or No  b b
instance Or Yes b Yes

class Not b b' | b -> b'
instance Not Yes No
instance Not No  Yes

-- This lets us enforce unambiguous mapping to a Coproduct below; again,
-- probably more ugly than necessary:
class AnyLeftMassageable xxs ys b | xxs ys -> b
instance (LeftMassageable xxs (x,xs) b)=> AnyLeftMassageable xxs (x,xs) b
instance (LeftMassageable xxs () b)=> AnyLeftMassageable xxs () b
instance (LeftMassageable xxs xs b0
         , AnyLeftMassageable xxs ys b1
         , Or b0 b1 b
         )=> AnyLeftMassageable xxs (Either xs ys) b

-- this defines the different instance options we can "select" depending on
-- whether the head is massageable product or not:
class MassageableToCoproduct' leftMassageable s t where  -- TODO: add (Coproduct s)=>
    massageNormalToCoproduct' :: leftMassageable -> s -> t

instance (Massageable xxs xsx
         , AnyLeftMassageable xxs ys No -- unambiguous mapping, else fails to typecheck
         )=> MassageableToCoproduct' Yes xxs (Either xsx ys) where -- TODO: enforce that tail is NOT massageable
    massageNormalToCoproduct' _ = Left . massageNormal

instance (Massageable xxs (x,xs)
         )=> MassageableToCoproduct' Yes xxs (x,xs) where
    massageNormalToCoproduct' _ = massageNormal

instance MassageableToCoproduct' Yes () () where -- TODO or combinable with above??
    massageNormalToCoproduct' _ = id

instance (MassageableToCoproduct xxs ys
            )=> MassageableToCoproduct' No xxs (Either xsx ys) where
    massageNormalToCoproduct' _ = Right . massageNormalToCoproduct -- N.B. not massageNormalToCoproduct'


-- our single instance class:
class MassageableToCoproduct s t where
    massageNormalToCoproduct :: s -> t

instance (MassageableToCoproduct' b s t, LeftMassageable s t b)=> MassageableToCoproduct s t where
    massageNormalToCoproduct = massageNormalToCoproduct' (undefined :: b)
