{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}  --these two for Isomorphic class
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   -- necessary for Shapely Generics instances
{-# LANGUAGE TypeOperators #-}       -- for our cons synonym
{-# LANGUAGE StandaloneDeriving #-}   -- these two for deriving AlsoNormal instances
{-# LANGUAGE UndecidableInstances #-}
module Data.Shapely.Compose
    where

import Data.Shapely
import Data.Shapely.Category
import Control.Applicative() -- Functor instances for (,) and Either


-- TODO: CHANGE NAMES TO SIMPLIFIED AND SUGGEST IMPORTING AS:
--          import Data.Shapely.Compose as Sh
--       SO WE CAN USE OPERATORS BUT AVOID NAME COLLISION
--
-- OTHER FUNCTIONS: (VERY USEFUL; DO NOW)
--     - length type func. (with type nats?)
--     - splitAt  (product take/drop in terms of this)
--     - popi (product (!!) in terms of this)
--     - replicate
--
--   complex, maybe not useful:
--     - intersperse
--     - intercalate
--     - transpose
--
--   products only: (USEFUL; DO LATER)
--     - zips
--     Homogenous only:
--       - and, or, sum, product, maximum, minimum
--       - folds, maps, scans
--       - sort/insert ?
-- -------
-- FUTURE TODOs:
--   -closed TypeFamilies for simpler instances
--   -type-level naturals for e.g. 'length;-like functions
--   -explore relationship to:
--     -lens
--     -Arrow
-- -------


-- TODO: see if we can simplify instances by using Only, instead of Either a (a,b)/()
--
-- | A singleton inhabited 'Coproduct'. This is an intermediate type useful for
-- constructing Conproducts, and in our instances (see e.g. 'Tail')
newtype Only a = Only a


-- TODO: consider making all Left parameters and results of sums explicitly () and (a,b)

-- | Deconstructive type functions:

-- ARE THESE ACTUALLY USEFUL??
type family Head xs
type family Tail xs
type instance Head (x,xs) = x
type instance Head (Either x xs) = x

type instance Tail (x,xs) = xs
type instance Tail (Either x (Either y ys)) = Either y ys 
type instance Tail (Either y ()) = Only ()
type instance Tail (Either y (a,b)) = Only (a,b)

type family Init xs
type instance Init (xN,()) = ()
type instance Init (x,(y,zs)) = (x, Init (y,zs)) 

type instance Init (Either x (Either y zs)) = x :<|: (Init (Either y zs))
type instance Init (Either x ()) = Only x
type instance Init (Either x (a,b)) = Only x

type family Last xs
type instance Last (x,()) = x
type instance Last (x,(y,zs)) = Last (y,zs) 

type instance Last (Either x (Either y zs)) = Last (Either y zs) 
type instance Last (Either x ()) = ()
type instance Last (Either x (a,b)) = (a,b)
type instance Last (Only b) = b

type family t :<|: ts
--type instance t :<|: ts = NormalConstr (NormalType ts) t ts
type instance x :<|: Only y = Either x y
type instance x :<|: Either y zs = Either x (Either y zs)
type instance x :<|: () = (x,())
type instance x :<|: (y,zs) = (x,(y,zs))

type family xs :|>: x
type instance () :|>: x = (x,())
type instance (x0,xs) :|>: x = (x0, xs :|>: x)

type instance Either x0 xs :|>: x = Either x0 (Tail (Either x0 xs) :|>: x)
type instance Only a :|>: b = Either a b -- TODO: insist on two instances for: b ~ () and b ~ (x,y)?


-- TODO: use instance defaulting here for these definitions after they are settled?
-- TODO: if we combine these methods into bigger classes, we should be able to leave out constraints right?


-- TODO: a simpler, less general type here:
-- | Note: @popl@ would be simply @id@.
--
-- > popr = swap . shiftr
popr :: (Symmetric (->) p, Shiftable t, ShiftedR t ~ p a b) => t -> (p b a)
popr = swap . shiftr


infixl 5 .|>
infixl 5 <|.

-- | A convenience operator for appending an element to a product type.
-- 'Shiftable' generalizes this operation.
--
-- > (.|>) = curry pushr
(.|>) :: (NormalType xs ~ Product, Shiftable (x,xs))=> xs -> x -> (xs :|>: x)
xs .|> x = shiftl (x,xs)

-- | A left push for Products.
--
-- > (<|.) = (,)
(<|.) ::  (NormalType xs ~ Product)=> x -> xs -> (x,xs)
(<|.) = (,)

-- | Class supporting normal type equivalent to list @concat@ function, for
-- coproducts as well as products
class Concatable xs where
    type Concated xs
    nconcat :: xs -> Concated xs

instance Concatable () where
    type Concated () = ()
    nconcat = id

instance (Concatable yss, Appendable xs (Concated yss), NormalType (Concated yss) ~ Product)=> Concatable (xs,yss) where
    type Concated (xs,yss) = xs :++: Concated yss
    nconcat = nappend . fmap nconcat

-- These are the two possible leaf Coproducts at the Last position of
-- our Either spine:
instance Concatable (Either () es) where
    type Concated (Either () es) = (Either () es)
    nconcat = id

instance Concatable (Either (x,ys) es) where
    type Concated (Either (x,ys) es) = (Either (x,ys) es)
    nconcat = id

-- TODO: restrict (Either x ys) to Coproduct
instance (Concatable ess,Appendable (Either x ys) (Concated ess))=> Concatable (Either (Either x ys) ess) where
    type Concated (Either (Either x ys) ess) = Either x ys :++: Concated ess
    nconcat = nappend . fmap nconcat



-- | Reversing Products and Coproducts
class Reversable t where
    type Reversed t
    type Reversed t = Reversed (Tail t) :|>: Head t
    nreverse :: t -> Reversed t

instance Reversable () where
    type Reversed () = ()
    nreverse = id

instance Reversable (Only x) where
    type Reversed (Only x) = (Only x)
    nreverse = id

instance (Reversable xs
         , Shiftable (x, Reversed xs)
         )=> Reversable (x,xs) where
    nreverse = shiftl . fmap nreverse 

instance (Reversable xs
         , Shiftable (Either x (Reversed xs))
         , (Tail (Either x (Reversed xs)) :|>: x)
           ~ (Reversed (Tail (Either x xs)) :|>: x) -- TODO improve this?
         )=> Reversable (Either x xs) where
    nreverse = shiftl . fmap nreverse 



-- | a class for shifting a sum or product left or right by one element, i.e. a
-- logical shift
class Shiftable t where
    shiftl :: t -> ShiftedL t
    shiftr :: t -> ShiftedR t
    
type ShiftedL t = Tail t :|>: Head t
type ShiftedR t = Last t :<|: Init t

swapFront :: Symmetric (->) p => p b (p a c) -> p a (p b c)
swapFront = associate . first swap . disassociate

instance Shiftable (x,()) where
    shiftl = id
    shiftr = id

-- TODO abstract these constraints better:
instance (Shiftable (y,zs)
        , Shiftable (x, zs)
        , ShiftedR (y,zs) ~ (Last (y, zs), Init (y, zs)) -- TODO replace with better constraint
        )=> Shiftable (x,(y,zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

instance Shiftable (Either a ()) where
    shiftl = swap
    shiftr = swap

instance Shiftable (Either a (x,y)) where
    shiftl = swap
    shiftr = swap

-- TODO: these constraints are wrong:
instance (Shiftable (Either y zs)
        , Shiftable (Either x zs)
        , (Tail (Either x zs) :|>: x)
          ~ (Tail (Either y zs) :|>: x) -- TODO: how can we make this obvious
        --- TODO: simplify these constraints:
        , (Last (Either y zs) :<|: (x :<|: Init (Either y zs))) 
          ~ Either a0 (Either x c0)
        , (Last (Either y zs) :<|: Init (Either y zs))
          ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

-- | A class supporting an N-ary 'uncurry' operation for applying functions to
-- 'Normal' products of matching arity.
class Uncurry t r where
    -- | A function capable of consuming the product @t@ and producing @r@.
    type t :->-> r
    puncurry :: (t :->-> r) -> t -> r

instance Uncurry (a,()) r where
    type (a,()) :->-> r = a -> r
    puncurry f = f . fst

instance (Uncurry (b,cs) r)=> Uncurry (a,(b,cs)) r where
    type (a,(b,cs)) :->-> r = a -> (b,cs) :->-> r
    puncurry f = uncurry (puncurry . f)


-- | Constructive type functions

class (NormalType xs ~ NormalType ys) => Appendable xs ys where -- TODO CONSTRAINT NECESSARY??
    type xs :++: ys   
    nappend :: (t ~ NormalConstr (NormalType xs))=> t xs ys -> (xs :++: ys)

instance (NormalType us ~ Product)=> Appendable () us where
    type () :++: us = us 
    nappend = snd

instance (NormalType us ~ Product, Appendable ts us)=> Appendable (a,ts) us where
    type (a,ts) :++: us = (a, ts :++: us) 
    nappend = fmap nappend . associate

instance (NormalType us ~ Coproduct)=> Appendable (Either a ()) us where
    type Either a () :++: us = Either a (Either () us)
    nappend = associate

instance (NormalType us ~ Coproduct)=> Appendable (Either a (b,c)) us where
    type Either a (b,c) :++: us = Either a (Either (b,c) us)
    nappend = associate

instance (Appendable (Either b ts) us)=> Appendable (Either a (Either b ts)) us where
    type Either a (Either b ts) :++: us = Either a (Either b ts :++: us)
    nappend = fmap nappend . associate


infixr 5 .++.
-- | A convenience operator for concating two product types.
-- 
-- > (.++.) = curry nappend
(.++.) :: (NormalType xs ~ Product, NormalType ys ~ Product, Appendable xs ys)=> xs -> ys -> xs :++: ys
(.++.) = curry nappend
--TODO: - our classes don't require the first constraint above
--      - how can we combine multiple clasues?


-- TAGS FOR PRODUCT / SUM TYPES:

-- TODO: BETTER TO MAKE THIS A CLASS (or add one) SO WE GET:
-- ( Product x )=> ... instead of...  (NormalType x ~ Product)=>
-- THEN WE COULD ADD CONSTRAINTS TO THE Product or Coproduct declaration
data Product
data Coproduct

type family NormalType t
type instance NormalType () = Product
type instance NormalType (x,ys) = Product
type instance NormalType (Either a b) = Coproduct
--type instance NormalType (Reversed t) = NormalType t -- ILLEGAL TODO

-- TODO: OR MAKE THE ARGUMENT TAKE THE TYPE DIRECTLY, SAME AS ABOVE?
type family NormalConstr t :: * -> * -> *
type instance NormalConstr Product = (,)
type instance NormalConstr Coproduct = Either

{- ASIDE -- ----
class (NormalConstr t ~ (,))=> Product t
instance Product ()
instance (Product ts)=> Product (a,ts)

class (NormalConstr e ~ Either)=> Coproduct e 
instance (Product t)=> Coproduct (Either t ())
instance (Product t, Product b)=> Coproduct (Either t (a,b))
instance (Product t, Coproduct (Either b c))=> Coproduct (Either t (Either b c))
-}
