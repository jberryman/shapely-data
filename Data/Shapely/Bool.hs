{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances, EmptyDataDecls #-}
module Data.Shapely.Bool
    where

-- Probably keep private for now, as with Data.Shapely.Category
-- Again, these borrowed from Oleg's HList work

-- Or use lifted constructors from DataKinds:
data True
data False

class And a b c | a b -> c
instance And True b b
instance And False  b False

class Or a b c | a b -> c
instance Or False  b b
instance Or True b True

class Not b b' | b -> b'
instance Not True False
instance Not False  True

