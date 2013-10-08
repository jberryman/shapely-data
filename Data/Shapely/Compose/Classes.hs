{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, UndecidableInstances #-}
module Data.Shapely.Compose.Classes
    where

import Data.Shapely.Classes

-- Internal module, mostly to avoid circular imports.





-- TODO: consider making all Left parameters and results of sums explicitly () and (a,b)

type family Head xs
type family Tail xs
type instance Head (x,xs) = x
type instance Head (Either x xs) = x
type instance Head (Only x) = x

type instance Tail (x,xs) = xs
type instance Tail (Either x (Either y ys)) = Either y ys 
type instance Tail (Either y ()) = Only ()
type instance Tail (Either y (a,b)) = Only (a,b)

type family Init xs
type instance Init (xN,()) = ()
type instance Init (x,(y,zs)) = (x, Init (y,zs)) 

type instance Init (Either x (Either y zs)) = x :< (Init (Either y zs))
type instance Init (Either x ()) = Only x
type instance Init (Either x (a,b)) = Only x

type family Last xs
type instance Last (x,()) = x
type instance Last (x,(y,zs)) = Last (y,zs) 

type instance Last (Either x (Either y zs)) = Last (Either y zs) 
type instance Last (Either x ()) = ()
type instance Last (Either x (a,b)) = (a,b)
type instance Last (Only b) = b

type family t :< ts
type instance x :< Only y = Either x y
type instance x :< Either y zs = Either x (Either y zs)
type instance x :< () = (x,())
type instance x :< (y,zs) = (x,(y,zs))

-- OUR USE OF UndecidableInstances:
--   Since we obviously intend these to be closed type families this isn't a
--   concern.
type family xs :> x
type instance () :> x = (x,())
type instance (x0,xs) :> x = (x0, xs :> x)

type instance Either x0 xs :> x = Either x0 (Tail (Either x0 xs) :> x)
type instance Only a :> b = Either a b -- TODO: insist on two instances for: b ~ () and b ~ (x,y)?



-- | A singleton inhabited 'Coproduct'. This is an intermediate type useful for
-- constructing Conproducts, and in our instances (see e.g. 'Tail')
newtype Only a = Only a

type instance NormalConstr (Only a) = Either
instance (Product t)=> Coproduct (Only t)

