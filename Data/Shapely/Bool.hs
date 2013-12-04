{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances, EmptyDataDecls , DataKinds , KindSignatures, TypeFamilies #-}
module Data.Shapely.Bool
    where

-- Probably keep private for now, as with Data.Shapely.Category
-- Again, these borrowed from Oleg's HList work


type family And (a :: Bool) (b :: Bool) :: Bool
type instance And True b = b 
type instance And b True = b 
type instance And False b = False -- N.B. coincident overlap
type instance And b False = False -- N.B. coincident overlap

type family Or (a :: Bool) (b :: Bool) :: Bool
type instance Or False b = b 
type instance Or b False = b 
type instance Or True b = True -- N.B. coincident overlap
type instance Or b True = True -- N.B. coincident overlap

type family Not (a :: Bool) :: Bool
type instance Not True = False
type instance Not False = True
