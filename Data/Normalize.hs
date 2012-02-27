{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Data.Normalize (
    mkNormable
  , Normable(..)
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | A class for types to be converted into a sort of "normal form" by
-- converting its constructors into a combination of @Either@, @(,)@
-- and @()@, and back again.
class Normable a b | a -> b, b -> a where
    toNorm   :: a -> b
    fromNorm :: b -> a


{-
 - NOTE:
 -     rename this something about normalization
 -         name: nermal
 -     types that are already in "normalized form" should be equal after transformations
 -     what about the notion of recursively normalizing arguments? deep nermalization
 -}


-- TODO: handle derived classes, decide about strict/NotStrict, records, etc.

-- | Generate a 'Nermalized' instance for the referenced types. E.g.
--
-- > $(nermalize ''Foo)
mkNormable :: Name -> Q [Dec]
mkNormable n = 
    do (TyConI d) <- reify n
       let (DataD cxt nm bndings cnstrctrs derivng) = d

       -- --------------------------------------------------------
       -- create the newtype wrapper for "normstructored" version
       let wrapperNm = mkName (nameBase nm ++ "Norm")  -- e.g. "FooNorm"
       normed <- normprods cnstrctrs
       let unwrapperName = mkName ("norm" ++ nameBase nm)
           wrapper = NewtypeD cxt wrapperNm bndings (RecC wrapperNm [(unwrapperName,NotStrict,normed)]) [] 
       -- --------------------------------------------------------
       -- build the normstructorable class instance for this type
       return [wrapper]

-- takes a list of constructors from the original type and returns a single
-- data type built using only (,), Either, and ()
-- This bit does the products, calling 'normsums' for each constructor
normprods :: [Con] -> Q Type
normprods [c]    = let (NormalC _ args) = c
                     in normsums $ map snd args
normprods (c:cs) = let (NormalC _ args) = c
                     in [t| Either $(normsums $ map snd args) $(normprods cs) |] 
normprods []     = [t| () |] -- only used on constructor-less types

-- convert a constructor into singleton type values, tuples and unit:
normsums :: [Type] -> Q Type
normsums [t]    = return t
normsums (t:ts) = fmap (AppT (AppT (TupleT 2) t)) (normsums ts)
normsums []     = [t| () |] -- only used for empty constructor, e.g. Nothing
