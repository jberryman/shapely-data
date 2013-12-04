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

import Data.Shapely.Normal.Exponentiation
import Data.Shapely.Normal.Classes



-- | Instances of the 'Shapely' class can be converted to and from a 'Normal'
-- representation, made up of @(,)@, @()@ and @Either@.
class (Exponent (Normal a))=> Shapely a where
    -- | A @Shapely@ instances \"normal form\" representation, consisting of
    -- nested product, sum and unit types. Types with a single constructor will
    -- be given a 'Product' Normal representation, where types with more than
    -- one constructor will be 'Coproduct's. 
    --
    -- See the documentation for 'deriveShapely', and the instances defined here
    -- for details.
    type Normal a

    to :: a -> Normal a

    from :: Normal a -> a
    from na = let a = fanin (constructorsOf a) na in a
    
    -- | Return a structure capable of rebuilding a type @a@ from its 'Normal'
    -- representation (via 'fanin').
    --
    -- This structure is simply the data constructor (or a 'Product' of
    -- constructors for 'Coproduct's), e.g. for @Either@:
    --
    -- > constructorsOf _ = (Left,(Right,()))
    --
    -- Satisfies:
    --
    -- > 'fanin' (constructorsOf a) (to a) == a
    constructorsOf :: a -> Normal a :=>-> a


-- Here I walk-through the ways we define 'Shapely' instances; you can also see
-- the documentation for mkShapely for a description.
--
-- Syntactically we can think of the type constructor (in this case (), but
-- usually, e.g. "Foo") becoming (), and getting pushed to the bottom of the
-- stack of its terms (in this case there aren't any).
instance Shapely () where  
    type Normal () = ()
    to = id
  --from = id
    constructorsOf _ = ()

-- And data constructor arguments appear on the 'fst' positions of nested
-- tuples, finally terminated by a () in the 'snd' place.
-- | Note, the normal form for a tuple is not itself
instance Shapely (x,y) where
    type Normal (x,y) = (x,(y,()))
    to (x,y) = (x,(y,()))
  --from (x,(y,())) = (x,y)
    constructorsOf _ = (,)

-- Here, again syntactically, both constructors Left and Right become `()`, and
-- we replace `|` with `Either` creating a sum of products.
instance Shapely (Either x y) where
    type Normal (Either x y) = Either (x,()) (y,())
    to = let f = flip (,) () in either (Left . f) (Right . f)
  --from = either (Left . fst) (Right . fst)
    constructorsOf _ = (Left,(Right,()))
  
-- The Normal form of a type is just a simple unpacking of the constructor
-- terms, and doesn't do anything to express recursive structure. But this
-- simple type function lets us work with different recursive structure in
-- additional classes, like Isomorphic.
instance Shapely [a] where 
    -- NOTE: data [] a = [] | a : [a]    -- Defined in `GHC.Types'
    type Normal [a] = Either () (a,([a],()))
    to []         = Left ()
    to ((:) a as) = Right (a, (as, ()))
    constructorsOf _ = ([],((:),()))

---- TODO  derive other instances of Prelude / etc. types here

{-
-- | A wrapper for recursive child occurences of a 'Normal'-ized type
newtype AlsoNormal a = Also { normal :: Normal a }
deriving instance (Show (Normal a))=> Show (AlsoNormal a)
deriving instance (Eq (Normal a))=> Eq (AlsoNormal a)
deriving instance (Ord (Normal a))=> Ord (AlsoNormal a)
deriving instance (Read (Normal a))=> Read (AlsoNormal a)
-}
