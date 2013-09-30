{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}  --these two for Isomorphic class
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   -- necessary for Shapely Generics instances
{-# LANGUAGE TypeOperators #-}       -- for our cons synonym
{-# LANGUAGE StandaloneDeriving #-}   -- these two for deriving AlsoNormal instances
{-# LANGUAGE UndecidableInstances #-}
module Data.Shapely.Classes
    where

-- internal module to avoid circular dependencies



-- | A Product is a list of arbitrary terms constructed with @(,)@, and
-- terminated by @()@ in the @snd@. e.g.
--
-- > prod = (1,(2,(3,())))
class (NormalConstr t ~ (,))=> Product t
instance Product ()
instance (Product ts)=> Product (a,ts)

-- | A coproduct is a non-empty list of 'Product's constructed with @Either@
-- and terminated by a 'Product' type on the @Right@. e.g.
--
-- > coprod = (Right $ Left (1,(2,(3,())))) :: Either (Bool,()) (Either (Int,(Int,(Int,()))) (Char,()))
--
-- To simplify type functions and class instances we also define the singleton
-- coproduct 'Only'.
class (NormalConstr e ~ Either)=> Coproduct e 
instance (Product t)=> Coproduct (Either t ())
instance (Product t, Product (a,b))=> Coproduct (Either t (a,b))
instance (Product t, Coproduct (Either b c))=> Coproduct (Either t (Either b c))

type family NormalConstr t :: * -> * -> *
type instance NormalConstr (a,b) = (,)
type instance NormalConstr () = (,)
type instance NormalConstr (Either a b) = Either



-- | Instances of the 'Shapely' class can be converted to and from a 'Normal'
-- representation, made up of @(,)@, @()@ and @Either@.
class Shapely a where
    -- | A @Shapely@ instances \"normal form\" representation, consisting of
    -- nested product, sum and unit types. Types with a single constructor will
    -- be given a 'Product' Normal representation, where types with more than
    -- one constructor will be 'Coproduct's. 
    --
    -- See the documentation for 'mkShapely', and the instances defined here
    -- for details.
    type Normal a
    toNorm :: a -> Normal a    -- TODO: rename toNormal, fromNormal. Or... to/from  ?
    fromNorm :: Normal a -> a

-- | Note, the normal form for a tuple is not itself
instance Shapely (x,y) where
    type Normal (x,y) = (x,(y,()))
    toNorm (x,y) = (x,(y,()))
    fromNorm (x,(y,())) = (x,y)

-- Syntactically we can think of the type constructor (in this case (), but
-- usually, e.g. "Foo") becoming (), and getting pushed to the bottom of the
-- stack of its terms (in this case there aren't any).
instance Shapely () where  
    type Normal () = ()
    toNorm = id
    fromNorm = id

-- Here, again syntactically, both constructors Left and Right become `()`, and
-- we replace `|` with `Either` creating a sum of products.
instance Shapely (Either x y) where
    type Normal (Either x y) = Either (x,()) (y,())
    toNorm = let f = flip (,) () in either (Left . f) (Right . f)
    fromNorm = either (Left . fst) (Right . fst)
  --fromNorm = Sh.fanin (Left,(Right,())).

---- TODO: more instances by hand. Check their equivalents in tests of TH code by using a newtype wrapper. -----

-- TODO: alternate names: Another, ChildNormal, RecNormal
-- | A wrapper for recursive child occurences of a 'Normal'-ized type
newtype AlsoNormal a = Also { normal :: Normal a }
deriving instance (Show (Normal a))=> Show (AlsoNormal a)
deriving instance (Eq (Normal a))=> Eq (AlsoNormal a)
deriving instance (Ord (Normal a))=> Ord (AlsoNormal a)
deriving instance (Read (Normal a))=> Read (AlsoNormal a)


-- | Two types @a@ and @b@ are isomorphic if their 'Normal' representations are
-- the same.
class (Shapely a, Shapely b, Normal a ~ Normal b)=> Isomorphic a b
instance (Shapely a, Shapely b, Normal a ~ Normal b)=> Isomorphic a b
