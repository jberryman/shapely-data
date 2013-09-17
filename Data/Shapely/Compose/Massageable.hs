{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Shapely.Compose.Massageable
    where

import Data.Shapely.Compose.Classes

-- An internal module mostly to keep use of OverlappingInstances isolated

-- needs: Coproduct/Product classes
--        Tail


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


-- TODO: rename "viewType"
-- TODO make a TIP class?
-- OR...
class PopSingle a l l' | a l -> l' where
    popSingle :: l -> (a,l')

instance (HasAny a l No)=> PopSingle a (a,l) l where
    popSingle = id

instance (PopSingle a l l', (x,l') ~ xl')=> PopSingle a (x,l) xl' where
    popSingle (x,l) = let (a,l') = popSingle l
                       in (a,(x,l'))



--TODO: restrict these to products/coproduct classes
class Massageable s t where
    massageNormal :: s -> t

instance Massageable () () where
    massageNormal = id

instance (Massageable s' l, PopSingle a (x,y) s')=> Massageable (x,y) (a,l) where
    massageNormal = fmap massageNormal . popSingle

-------------
-- THIS IS ALLRIGHT, BECAUSE:
--  - most record types only make sense for products (since you get partial funcs otherwise)
--  - generally sum types are pattern-matched against.

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

