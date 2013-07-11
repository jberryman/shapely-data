{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}  --these two for Isomorphic class
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}   -- necessary for Shapely Generics instances
{-# LANGUAGE TypeOperators #-}       -- for our cons synonym
{-# LANGUAGE StandaloneDeriving #-}   -- these two for deriving AlsoNormal instances
{-# LANGUAGE UndecidableInstances #-}
module Data.Shapely
    where

-- TODO: export TH functionality here
--import Data.Shapely.TH


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
    -- | A @Shapely@ instances "normal form" representation consisting of
    -- nested product, sum and unit types. TODO: document how we do that here
    type Normal a
    toNorm :: a -> Normal a
    fromNorm :: Normal a -> a

-- | Note, the normal form for a tuple is not itself, unlike @Either@ and @()@.
instance Shapely (x,y) where
    type Normal (x,y) = (x,(y,()))
    toNorm (x,y) = (x,(y,()))
    fromNorm (x,(y,())) = (x,y)

instance Shapely (Either x y) where
    type Normal (Either x y) = Either x y
    toNorm = id
    fromNorm = id

instance Shapely () where  
    type Normal () = ()
    toNorm = id
    fromNorm = id
---- TODO: more instances by hand. Check their equivalents in tests of TH code. -----

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

-- | Convert a type @a@ to an isomorphic type @b@.
--
-- > coerce = fromNorm . toNorm
coerce :: (Isomorphic a b)=> a -> b
coerce = fromNorm . toNorm

-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = fromNorm . f . toNorm
infixr 1 $$  --is this right?
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = fromNorm . f . toNorm

-- TODO: polymorphic application combinator of type:
--    :: (a -> (b,c))     or (a -> Either b c) 
--    -> (a -> (b,(c,())) or (a -> Either (b,()) (c,())
-- to provide ease of use with functions from e.g. Arrow
-- Isn't this basically: fmap toNorm  ?
--
