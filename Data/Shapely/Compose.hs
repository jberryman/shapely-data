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

-- | A singleton inhabited 'Coproduct'. This is an intermediate type useful for
-- constructing Conproducts, and in our instances (see e.g. 'Tail')
newtype Only a = Only a


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

type instance Init (Either x (Either y zs)) = x :<|: Init (Either y zs)
--type instance Init (Either x (Either y zs)) = Either x (Init (Either y zs))
type instance Init (Either x ()) = Only x
type instance Init (Either x (a,b)) = Only x

type family Last xs
type instance Last (x,()) = x
type instance Last (x,(y,zs)) = Last (y,zs) 

type instance Last (Either x (Either y zs)) = Last (Either y zs) 
type instance Last (Either x ()) = ()
type instance Last (Either x (a,b)) = (a,b)

type family t :<|: ts
--type instance t :<|: ts = NormalConstr (NormalType ts) t ts
type instance x :<|: Only y = Either x y
type instance x :<|: Either y zs = Either x (Either y zs)
type instance x :<|: () = (x,())
type instance x :<|: (y,zs) = (x,(y,zs))

type family xs :|>: x
type instance () :|>: x = (x,())
type instance (x0,xs) :|>: x = (x0, xs :|>: x)

type instance Either y zs :|>: x = Either y (Tail (Either x zs) :|>: x)
type instance Only a :|>: b = Either a b

 -- TODO remove class and define as = popr = swap . shiftr
{-
-- | A class supporting a \"pop\" operation off the right end of a product or
-- coproduct. The equivalent left pop is simply 'id'
class Poppable xs where
    popr :: (t ~ NormalConstr (NormalType xs))=> xs -> t (Init xs) (Last xs)
  --popl = id

instance Poppable (x,()) where
    popr = swap

instance (Poppable (y,zs), NormalType zs ~ Product)=> Poppable (x,(y,zs)) where
    popr = disassociate . fmap popr

instance Poppable (Either x ()) where
    popr = id

instance Poppable (Either x (a,b)) where
    popr = id

instance (Poppable (Either y zs))=> Poppable (Either x (Either y zs)) where
    popr = disassociate . fmap popr
-}

-- TODO pushr is just shiftr. So remove this, replace implementation of Shiftable, keep product push function
{-
-- | A class supporting a \"push\" operation onto the right end of a product or
-- coproduct. The equivalent left push would be simply 'id'
class Pushable xs x where
    pushr :: (t ~ NormalConstr (NormalType xs))=> t xs x -> (xs :|>: x)
    --pushl :: (t ~ NormalConstr (NormalType xs))=> t x xs -> t x xs
 
instance Pushable () x where
    pushr = swap

instance (NormalType xs ~ Product, Pushable xs x)=> Pushable (x0,xs) x where
    pushr = fmap pushr . associate

instance (NormalType x ~ Product)=> Pushable (Either a ()) x where
    pushr = associate

instance (NormalType x ~ Product)=> Pushable (Either a (b,c)) x where
    pushr = associate

instance (Pushable (Either b ts) x)=> Pushable (Either a (Either b ts)) x where
    pushr = fmap pushr . associate

infixl 5 .|>
infixl 5 <|.

-- | A convenience operator for appending an element to a product type.
--
-- > (.|>) = curry pushr
(.|>) :: (NormalType xs ~ Product, Pushable xs x)=>xs -> x -> (xs :|>: x)
(.|>) = curry pushr

-- | A left push for Products.
--
-- > (<|.) = (,)
(<|.) ::  (NormalType xs ~ Product)=> x -> xs -> (x,xs)
(<|.) = (,)
-}

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


-- TODO: every Reversable is Pushable, etc. fix hierarchy so we have cleaner type sigs here
-- | Reversing lists
class Reversable l where
    type Reversed l
    nreverse :: l -> Reversed l

{-
instance Reversable () where
    type Reversed () = ()
    nreverse = id

-- TODO: DEFINE IN TERMS OF SHIFTR AND SHIFTL
-- instance Reversable t where
--     type Reversed t = Reversed (Tail t) :|>: Head t
--     nreverse = shiftl . fmap nreverse -- PROBLEM: no base case!
-- TODO: recreate relevant copy of categories in sub-module, use internally (don't export).
-- TODO: is this efficient? what does this even look like compiled?
-- TODO: how can we avoid: Shiftable (x, Reversed xs) constraint
instance Reversable (x,xs) where
--instance (NormalType (Reversed xs) ~ Product, Shiftable (x, Reversed xs), Reversable xs)=> Reversable (x,xs) where
    type Reversed (x,xs) = Reversed xs :|>: x
    nreverse = shiftl . fmap nreverse

instance Reversable (Either a ()) where
    type Reversed (Either a ()) = Either () a
    nreverse = swap

instance Reversable (Either a (x,y)) where
    type Reversed (Either a (x,y)) = Either (x,y) a
    nreverse = swap

-- TODO switch the ordering of args to pushr... WHICH WOULD GIVE US SHIFTR!
instance Reversable (Either a (Either x ys)) where
-- instance (NormalType (Either x ys) ~ Coproduct, NormalConstr (NormalType (Reversed (Either x ys))) ~ Either, Shiftable (Either a (Reversed (Either x ys))), Reversable (Either x ys))=> Reversable (Either a (Either x ys)) where
    type Reversed (Either a (Either x ys)) = Reversed (Either x ys) :|>: a
    nreverse = shiftl . fmap nreverse
-} ------- TODO try defining more general instances below, in terms of Tail, etc. ------------

-- | a class for shifting a sum or product left or right by one element, i.e. a
-- logical shift
class Shiftable t where
    -- type ShiftedL t
    -- type ShiftedL t = Tail t :|>: Head t
    -- type ShiftedR t
    -- type ShiftedR t = Last t :<|: Init t
    shiftl :: t -> ShiftedL t
    shiftr :: t -> ShiftedR t

type ShiftedL t = Tail t :|>: Head t
type ShiftedR t = Last t :<|: Init t

{-
-- NOTE: Thought about letting this subsume Pushable/Poppable
-- associate $ first swap $ disassociate
instance (Pushable (Tail s) (Head s), Poppable s)=> Shiftable s where
    shiftl = pushr . swap
    shiftr = swap . popr
-}

instance Shiftable (x,()) where
    shiftl = id
    shiftr = id

swapFront :: Symmetric (->) p => p b (p a c) -> p a (p b c)
swapFront = associate . first swap . disassociate


-- TODO abstract these constraints better:
instance (Shiftable (y,zs)
        , Shiftable (x, zs)
        , ShiftedR (y,zs) ~ (Last (y, zs), Init (y, zs))
        )=> Shiftable (x,(y,zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

instance Shiftable (Either a ()) where
    shiftl = swap
    shiftr = swap

instance Shiftable (Either a (x,y)) where
    shiftl = swap
    shiftr = swap


instance (Shiftable (Either y zs)
        , Shiftable (Either x zs)
        , ShiftedR (Either y zs) ~ Either (Last (Either y zs)) (Init (Either y zs))
        -- TODO: how can we espress this and why is this necessary here but not in (,) above?
        --       if we associate Last with a class that lets us constrain outer constructer to match??
        --       Or if we make ShiftedL/R associated type synonyms, too?
        , (Last (Either y zs) :<|: (x :<|: Init (Either y zs))) ~ Either (Last (Either y zs)) (Either x (Init (Either y zs)))
        )=> 
        Shiftable (Either x (Either y zs)) where
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
