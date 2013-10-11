{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances #-}
module Data.Shapely.Bool
    where

-- Probably keep private for now, as with Data.Shapely.Category

-- Again, these borrowed from Oleg's HList work
data Yes
data No

class And a b c | a b -> c
instance And Yes b b
instance And No  b No

class Or a b c | a b -> c
instance Or No  b b
instance Or Yes b Yes

class Not b b' | b -> b'
instance Not Yes No
instance Not No  Yes

