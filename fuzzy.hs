{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Shapely
import Data.Shapely.Compose

{-
class HOccurs e l
 where
  hOccurs :: l -> e


instance ( Product l
         , HOccursNot e l
         )
           => HOccurs e (e,l)
 where
  hOccurs (e,_) = e

instance ( HOccurs e l
         , Product l
         )
           => HOccurs e (e',l)
 where
  hOccurs  (_,l) = hOccurs l

  -}

class  TypeEq x y b | x y -> b
instance TypeEq x x Yes
instance b ~ No => TypeEq x y b



{- TO BEAT POLYMORPHISM, can we use this?
class TypeCast x y | x -> y, y -> x
 where
  typeCast :: x -> y
-}

data Yes = Yes deriving (Show,Eq)
data No  = No deriving (Show,Eq)

-- testing:
type family Not b
type instance Not Yes = No
type instance Not No = Yes

----- THIS ISN'T NECESSARY PROBABLY:

class HasSingle a l b | a l -> b 
  where
    hasSingle :: a -> l -> b

{-
instance (HasAny a l No)=> HasSingle a (a,l) Yes 
  where
    hasSingle _ _ = Yes
-}
instance (HasAny a l b', b ~ Not b')=> HasSingle a (a,l) b
  where
    hasSingle _ _ = undefined -- THIS METHOD PROBABLY NOT NECESSARY

instance (HasSingle a l b)=> HasSingle a (x,l) b ------ or b ~ b'  ?
  where
    hasSingle a (_,l) = hasSingle a l

instance HasSingle a () No
  where
    hasSingle _ _ = No

-- --------------

class HasAny a l b | a l -> b
  where
    hasAny :: a -> l -> b -- METHOD UNNECESSARY

instance HasAny a (a,l) Yes
  where
    hasAny _ _ = Yes

instance (HasAny a l b)=> HasAny a (x,l) b
-- instance (b ~ b', HasAny a l b')=> HasAny a (x,l) b
  where
    hasAny a (_,l) = hasAny a l

instance HasAny a () No
  where
    hasAny _ _ = No

-- for 'Coproduct's:
instance HasAny p (Either p ps) Yes
  where
    hasAny _ _ = Yes

instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
    where
      hasAny = undefined

instance HasAny p (Only p) Yes
  where
    hasAny _ _ = Yes
instance (b ~ No)=> HasAny p (Only x) b
  where
    hasAny _ _ = No

-- --------------

{-
class (HasSingle a l Yes)=> PopType a l where -- NEED to use another param, not typefunc here
    type Sans l a
    popOne :: l -> (a, Sans l a)

instance PopType a (a,l) where
    type Sans (a,l) a = l
    popOne (a,l) = (a,l)

instance (HasSingle a l Yes)=> PopType a (x,l) where
    type Sans (x,l) a = (x, Sans l a)
    popOne (x,l) = let (a,l) = popOne l
                    in (a, (x,l))
                    -}

-- --------------

class GetSingle a l where
    getSingle :: l -> a

instance (HasAny a l No)=> GetSingle a (a,l) where
    getSingle (a,_) = a

instance (GetSingle a l)=> GetSingle a (x,l) where
    getSingle (_,l) = getSingle l


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



-- --------------
-- TODO: I think we need getSingle to "pop" so that we can do instance ConvertProduct () ()
{-
class ConvertProduct s t where
    fuzzProd :: s -> t

instance ConvertProduct s () where
    fuzzProd _ = ()

-- product -> product is BIJECTION
-- coproduct -> coproduct is TOTAL SURJECTIVE

-- THIS DISALLOWS DUPLICATE ELEMENTS IN THE SOURCE - good, no ambiguity
-- THIS ALLOWS DROPPING ELEMENTS FROM SOURCE       - BAD! all sources are valid for the target ()
-- THIS ALLOWS DUPLICATE ELEMENTS IN THE TARGET    - bad! this is probably never useful
instance (ConvertProduct s l, GetSingle a s)=> ConvertProduct s (a,l) where
    fuzzProd s = (getSingle s, fuzzProd s)
-}

class ConvertProduct s t where
    fuzzProd :: s -> t

instance ConvertProduct () () where
    fuzzProd () = ()

-- product   -> product is BIJECTION
-- coproduct -> coproduct is TOTAL SURJECTIVE (product should be treated as singleton coproduct in this respect)
--    ...and provide special cases for products in either source or target above, 
--    in which case we treat as singleton coproduct.
instance (ConvertProduct s' l, PopSingle a s s')=> ConvertProduct s (a,l) where
    fuzzProd = fmap fuzzProd . popSingle

--------- Now we'll try general convert:

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
-- MAYBE WE CAN MAKE 'ONLY' INJECTIVE ASSOC. DATA
-- OR WE CAN MAKE MassageProduct MassageCoproduct helper classes
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

{-
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
    -}

class UnwrapOnly t where
    type UnwrappedOnly t
    unwrapOnly :: t -> UnwrappedOnly t

instance UnwrapOnly (Only t) where
    type UnwrappedOnly (Only t) = t
    unwrapOnly (Only p) =  p

instance UnwrapOnly (Either p ps) where
    type UnwrappedOnly (Either p ps) = Either p ps
    unwrapOnly = id
