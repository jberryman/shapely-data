{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Shapely.Compose.Massage
    where

import Data.Shapely.Compose

-- An internal module mostly to keep use of OverlappingInstances isolated



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
class Massage s t where
    massage :: s -> t

instance Massage () () where
    massage = id

instance (Massage s' l, PopSingle a (x,y) s')=> Massage (x,y) (a,l) where
    massage = fmap massage . popSingle

-------------
-- we might need to insist on only fuzzy coproducts (with product ordering being significant)
-- THIS MIGHT BE ALLRIGHT, BECAUSE:
--  - most record types only make sense for products (since you get partial funcs otherwise)
--  - generally sum types are pattern-matched against.

-- instance (Massage s (Either t ts)
--          , Massage (Tail (Either s ss)) (Either t ts)
--          , EitherTail ss
--          )=> Massage (Either s ss) (Either t ts) where
--     massage = eitherTail massage massage -- EITHERTAIL NOT NECESSARY! as final will be: 
--                                          --    (x,y) (Either t ts)
instance (Massage s (Either t ts), Massage ss (Either t ts)
         )=> Massage (Either s ss) (Either t ts) where
    massage = either massage massage

-- UNNECESSARY!:
-- instance (Massage p (Either t ts))=> Massage (Only p) (Either t ts) where
--     massage (Only p) = massage p  

-- base cases:
instance (HasAny (x,y) (Tail (Either (x,y) ts)) No)=> Massage (x,y) (Either (x,y) ts) where
    massage = Left

{-
instance (Massage (x,y) (Tail (Either t ts))
         )=> Massage (x,y) (Either t ts) where
    -- massage = Right . unwrapOnly . massage
-}
-- TODO MAYBE WE CAN MAKE 'ONLY' INJECTIVE ASSOC. DATA
-- INSTEAD OF MassageProduct MassageCoproduct helper classes
--      
instance (MassageCoproduct (x,y) ts)=> Massage (x,y) (Either t ts) where
    -- Drop into a 'massage' that observers product ordering, for when we
    -- hit the base case (x,y) (x',y'):
    massage = massageCoproduct

-- HELPER. unexported.
class MassageCoproduct p ts where
    massageCoproduct :: p -> ts
instance MassageCoproduct (x,y) (x,y) where
    massageCoproduct = id
instance MassageCoproduct (x,y) (Either (x,y) ts) where
    massageCoproduct = Left
instance (MassageCoproduct (x,y) ts)=> MassageCoproduct (x,y) (Either t ts) where
    massageCoproduct = Right . massageCoproduct


instance Massage (x,y) (Only (x,y)) where
    massage = Only

-- we want to treat source as sum of TIPs when target is prod:
instance (Massage s (x,y), Massage ss (x,y))=> Massage (Either s ss) (x,y) where
    massage = either massage massage

-- UNNECESSARY!:
-- instance (Massage s (x,y))=> Massage (Only s) (x,y) where
--     massage (Only s) = massage s

{- SCRATCH....
 
-- TODO maybe move into eitherTail class?
--      what about products?
class Untail a t where
    type Untailed a t
    untail :: t -> Untailed a t

instance Untail a (Only p) where
    type Untailed a (Only p) = Either a p
    untail (Only p) = Right p

instance Untail a (Either p ps) where
    type Untailed a (Either p ps) = Either a (Either p ps)
    untail = Right

-- or...

class UnwrapOnly t where
    type UnwrappedOnly t
    unwrapOnly :: t -> UnwrappedOnly t

instance UnwrapOnly (Only t) where
    type UnwrappedOnly (Only t) = t
    unwrapOnly (Only p) =  p

instance UnwrapOnly (Either p ps) where
    type UnwrappedOnly (Either p ps) = Either p ps
    unwrapOnly = id
     -}
