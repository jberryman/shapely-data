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

import Data.Shapely.Normal.FannedApplication
import Data.Shapely.Normal.Classes



-- | Instances of the 'Shapely' class can be converted to and from a 'Normal'
-- representation, made up of @(,)@, @()@ and @Either@.
class (Fanin (Normal a) a)=> Shapely a where
    -- | A @Shapely@ instances \"normal form\" representation, consisting of
    -- nested product, sum and unit types. Types with a single constructor will
    -- be given a 'Product' Normal representation, where types with more than
    -- one constructor will be 'Coproduct's. 
    --
    -- See the documentation for 'mkShapely', and the instances defined here
    -- for details.
    type Normal a

    to :: a -> Normal a

    from :: Normal a -> a
    from na = let a = fanin (constructorsOf a) na in a
    
    -- NOTE: I originally wanted simply: `constructors :: Normal a :->-> a`
    -- but inferrence seems broken there, even w/ scoped type variables.
    constructorsOf :: a -> Normal a :->-> a


-- Syntactically we can think of the type constructor (in this case (), but
-- usually, e.g. "Foo") becoming (), and getting pushed to the bottom of the
-- stack of its terms (in this case there aren't any).
instance Shapely () where  
    type Normal () = ()
    to = id
  --from = id
    constructorsOf _ = ()

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
    
---- TODO: list instance by hand. Check their equivalents in tests of TH code by using a newtype wrapper. -----



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
