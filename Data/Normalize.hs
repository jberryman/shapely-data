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
 -     how will recursive types work?
 -     what about the notion of recursively normalizing arguments? deep nermalization? 
 -         the conversion should be re-cursive, in terms of our newtype wrapper
 -}


-- TODO HERE: handle derived classes, decide about strict/NotStrict, records, make
--            variable names prettier

-- | Generate a 'Normable' instance for the referenced types. E.g.
--
-- > $(mkNormable ''Foo)  -- '' references a TH "Name"
--
-- This requires the @TemplateHaskell@ extension enabled.
mkNormable :: Name -> Q [Dec]
mkNormable n = 
    do (TyConI d) <- reify n
       let (DataD cxt nm bndings cnstrctrs derivng) = d

       -- --------------------------------------------------------
       -- create the newtype wrapper for "normstructored" version
       let wrapperNm = mkName (nameBase nm ++ "Norm")  -- e.g. "FooNorm"
       normed <- normprods (nm,wrapperNm) cnstrctrs
       let unwrapperName = mkName ("norm" ++ nameBase nm)
           wrapper = NewtypeD cxt wrapperNm bndings (RecC wrapperNm [(unwrapperName,NotStrict,normed)]) [] 

       -- --------------------------------------------------------
       -- build the Normable class instance for this type
       return [wrapper]



-- takes a list of constructors from the original type and returns a single
-- data type built using only (,), Either, and ()
-- This bit does the products, calling 'normsums' for each constructor
normprods :: (Name,Name) -> [Con] -> Q Type
normprods nms [c] = let (NormalC _ args) = c
                     in normsums nms args
normprods nms (c:cs) = let (NormalC _ args) = c
                        in [t| Either $(normsums nms args) $(normprods nms cs) |] 
normprods nms [] = [t| () |] -- only used on constructor-less types

-- convert a constructor into singleton type values, tuples and unit:
normsums :: (Name,Name) -> [StrictType] -> Q Type
normsums (nm,wrapperNm) = nsums . map (replaceRec . snd) where
    nsums [t]    = return t
    nsums (t:ts) = fmap (AppT (AppT (TupleT 2) t)) (nsums ts)
    nsums []     = [t| () |] -- only used for empty constructor, e.g. Nothing
    
    -- NOTE: we replace every visible instance of the original type with the
    -- normalized form for recursive types. This might not be the best
    -- approach.
    replaceRec = namemap (\n -> if n == nm then wrapperNm else n)

-- traverse a Type tree, modifying names:
namemap :: (Name -> Name) -> Type -> Type
namemap f (ForallT bs cxt t) = (ForallT bs cxt $ namemap f t)
namemap f (AppT t1 t2) = AppT (namemap f t1) (namemap f t2)
namemap f (SigT t k) = SigT (namemap f t) k
namemap f (ConT n) = ConT $ f n
namemap f (VarT n) = VarT $ f n
namemap _ t = t
