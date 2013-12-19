{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- nested type families: 
                                       --     Init (Either x (Either y zs)) = x :< Init (Either y zs)
                                       --     Either x0 xs :> x = Either x0 (Tail (Either x0 xs) :> x)
module Data.Shapely.Normal.Classes
    where


-- Internal module, mostly to avoid circular imports.


-- TODO
--   What if we make a class that bundles all the relations we can say between
--   these type funcs, e.g.
--       Head t :< Tail t ~ t
--   And we could make Sum/Product sub-classes. Would that make fmapTail
--   more viable?



-- | A Product is a list of arbitrary terms constructed with @(,)@, and
-- terminated by @()@ in the @snd@. e.g.
--
-- > prod = (1,(2,(3,())))
class (NormalConstr t ~ (,))=> Product t
instance Product ()
instance (Product ts)=> Product (a,ts)

-- | A @Sum@ is a non-empty list of 'Product's constructed with @Either@
-- and terminated by a 'Product' type on the @Right@. e.g.
--
-- > s = (Right $ Left (1,(2,(3,())))) :: Either (Bool,()) (Either (Int,(Int,(Int,()))) (Char,()))
--
-- To simplify type functions and class instances we also define the singleton
-- sum 'Only'.
class (NormalConstr e ~ Either)=> Sum e 
instance (Product t)=> Sum (Either t ())
instance (Product t, Product (a,b))=> Sum (Either t (a,b))
instance (Product t, Sum (Either b c))=> Sum (Either t (Either b c))

type family NormalConstr t :: * -> * -> *
type instance NormalConstr (a,b) = (,)
type instance NormalConstr () = (,)
type instance NormalConstr (Either a b) = Either


-- TODO look at which of these we use, and reassess definitions and names in light of algebraic approach 
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

-- Non-algebraic: add product to a sum, or multiply term onto product: --TODO still needed?
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
type instance Only a :> b = Either a b



-- | A singleton inhabited 'Sum'. This is an intermediate type useful for
-- constructing Conproducts, and in our instances (see e.g. 'Tail')
newtype Only a = Only { just :: a }

type instance NormalConstr (Only a) = Either
instance (Product t)=> Sum (Only t)

