{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Shapely.Compose.Massageable
    where

import Data.Shapely.Compose.Classes

-- An internal module mostly to keep use of OverlappingInstances isolated


-- We borrow this type-equality comparison trick from Oleg: 
--   http://okmij.org/ftp/Haskell/ConEQ.hs
data Yes = Yes deriving (Show,Eq)
data No  = No deriving (Show,Eq)

class HasAny a l b | a l -> b

instance HasAny a (a,l) Yes
instance (HasAny a l b)=> HasAny a (x,l) b
-- instance (b ~ b', HasAny a l b')=> HasAny a (x,l) b
instance HasAny a () No

-- for 'Coproduct's:
instance HasAny p (Either p ps) Yes
instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
instance HasAny p (Only p) Yes
instance (b ~ No)=> HasAny p (Only x) b

class TIP a l l' | a l -> l' where
    viewType :: l -> (a,l')

instance (HasAny a l No)=> TIP a (a,l) l where
    viewType = id

instance (TIP a l l', (x,l') ~ xl')=> TIP a (x,l) xl' where
    viewType (x,l) = let (a,l') = viewType l
                      in (a,(x,l'))



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

instance (Massageable s (Either t ts), Massageable ss (Either t ts)
         )=> Massageable (Either s ss) (Either t ts) where
    massageNormal = either massageNormal massageNormal

-- base cases:
instance (HasAny (x,y) (Tail (Either (x,y) ts)) No)=> Massageable (x,y) (Either (x,y) ts) where
    massageNormal = Left

instance (MassageableCoproduct (x,y) ts)=> Massageable (x,y) (Either t ts) where
    -- Drop into a 'massage' that observers product ordering, for when we
    -- hit the base case (x,y) (x',y'):
    massageNormal = massageCoproduct


-- HELPER. unexported.
class MassageableCoproduct p ts where
    massageCoproduct :: p -> ts
instance MassageableCoproduct (x,y) (x,y) where
    massageCoproduct = id
instance MassageableCoproduct (x,y) (Either (x,y) ts) where
    massageCoproduct = Left
instance (MassageableCoproduct (x,y) ts)=> MassageableCoproduct (x,y) (Either t ts) where
    massageCoproduct = Right . massageCoproduct

instance Massageable (x,y) (Only (x,y)) where
    massageNormal = Only

-- we want to treat source as sum of TIPs when target is prod:
instance (Massageable s (x,y), Massageable ss (x,y))=> Massageable (Either s ss) (x,y) where
    massageNormal = either massageNormal massageNormal
