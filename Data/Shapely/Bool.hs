{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances, EmptyDataDecls , DataKinds , KindSignatures #-}
module Data.Shapely.Bool
    where

-- Probably keep private for now, as with Data.Shapely.Category
-- Again, these borrowed from Oleg's HList work

--TODO why not type families? And making use of coincident overlap, e.g.
--     type instance And True b = b ; And b True = b ; And False b = False ; And b False = False

class And (a :: Bool) (b :: Bool) (c :: Bool) | a b -> c
instance And True b b
instance And False  b False

class Or (a :: Bool) (b :: Bool) (c :: Bool) | a b -> c
instance Or False  b b
instance Or True b True

class Not (b :: Bool) (b' :: Bool) | b -> b'
instance Not True False
instance Not False  True

