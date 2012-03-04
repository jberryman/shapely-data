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
 -     types that are already in "normalized form" should be equal after transformations
 -     Call this something like "shapely" and change to Data.Shape(ly)
 -
 -     Structural Representation of types.
 -
 -     These can be ambiguous, since an argument to an initial type might be ()
 -     for example, so the conversion back to the original depends on the arity
 -     of the constructor. E.g.:
 -         Right ()   --> Right ()
 -                    --> Nothing
 -
       ambiguity in our class conversions is avoided since each has a newtype
       wrapper

       TODO:
       - finish code generation
       - change name to something having to do with: structure, shape, ...
       - re-write rules from/to = id, etc.
 -}


-- TODO HERE: handle derived classes, decide about strict/NotStrict, records, make
--            variable names prettier

-- | Generate a 'Normable' instance for the referenced types. E.g.
--
-- > $(mkNormable ''Foo)  -- '' references a TH "Name"
--
-- This requires the @TemplateHaskell@ extension to be enabled.
mkNormable :: Name -> Q [Dec]
mkNormable n = 
    do (TyConI d) <- reify n  -- what about PrimTyconI? should we represent literals as newtype-wrapped literals?
       let (DataD cxt nm bndings cnstrctrs derivng) = d

       -- --------------------------------------------------------
       -- create the newtype wrapper for "normstructored" version
       let wrapperNm = mkName (nameBase nm ++ "Norm")  -- e.g. "FooNorm"
       normed <- normprods (nm,wrapperNm) cnstrctrs
       let unwrapperName = mkName ("norm" ++ nameBase nm)
           wrapper = NewtypeD cxt wrapperNm bndings (RecC wrapperNm [(unwrapperName,NotStrict,normed)]) [] 

       -- --------------------------------------------------------
       -- build the Normable class instance for this type
       frmClauses <- fromNormClauses cnstrctrs
       --let fromNormDec = FunD 'fromNorm frmClauses
       let fromNormDec = FunD 'fromNorm $ 
             [Clause [VarP $ mkName "a"] (NormalB (AppE (VarE $ mkName "fromNorm'") (AppE (VarE unwrapperName) (VarE $ mkName "a")))) 
                [FunD (mkName "fromNorm'") frmClauses] ]

       let toNormDec = FunD 'toNorm (toNormClauses cnstrctrs)

       let normableInstance = InstanceD [] normableTs [toNormDec, fromNormDec]
           normableTs = (ConT ''Normable) `AppT` (decToType d) `AppT` (decToType wrapper)
       return [wrapper,normableInstance]

-- this is no fun... :-(
decToType :: Dec -> Type
decToType (DataD _ nm bndings _ _)    = d2t (ConT nm) bndings
decToType (NewtypeD _ nm bndings _ _) = d2t (ConT nm) bndings
d2t t bs = foldl AppT t $ map (VarT . bndNames) bs 
bndNames (PlainTV n) = n
bndNames (KindedTV n _) = n



-- -------------------------
-- DATA CONVERSION HELPERS:


-- FROMNORM METHOD: --

-- takes list of constructors to CONVERT TO, pattern matching against Either, (), (,)
fromNormClauses :: [Con] -> Q [Clause]
fromNormClauses cs = do bdies <- mapM fromNormBdy cs
                        let pats = fromNormPats cs
                        return $ zipWith (\p bdy-> Clause [p] bdy []) pats bdies

-- for each constructor, return the pattern to match 
fromNormPats :: [Con] -> [Pat]
fromNormPats [_] = [sumPat]
fromNormPats (_:cs) = ConP 'Left [sumPat] : (map wrapRightPat $ fromNormPats cs)
fromNormPats [] = error "type has no constructors"

sumPat = VarP $ mkName "sumVar"
sumExp = VarE $ mkName "sumVar"
wrapRightPat p = ConP 'Right [p] 

-- given pattern match above, return a body that de-tuples args into Con
fromNormBdy :: Con -> Q Body
fromNormBdy (NormalC nm sts) = fmap NormalB $ deTuple $ length sts
    where deTuple :: Int -> Q Exp -- function applying n-kind constructor to n-nested tuples
          deTuple 0 = return (ConE nm) -- empty constructor (e.g. Nothing) 
          -- IF WE DECIDE TO, RECURSIVE TYPE CONVERSION HAPPENS HERE:
          deTuple n = [| $(deTupleN n) $(return $ ConE nm) $(return sumExp) |] -- 'sumVar' is our bound tuple above
          deTupleN 1 = [| \constr a-> constr a |]
          deTupleN n = [| \constr (a,b)-> $(deTupleN (n-1)) (constr a) b |]


-- TONORM METHOD: --

-- takes a list of constructors to CONVERT FROM (pattern match against)
toNormClauses :: [Con] -> [Clause]
toNormClauses _ = [Clause [WildP] (NormalB (VarE 'undefined)) []]

-- -------------------------
-- TYPE CONVERSION HELPERS:

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
normsums (nm,wrapperNm) = nsums . map snd where
--normsums (nm,wrapperNm) = nsums . map (replaceRec . snd) where -- RECURSIVE TYPE CONVERSION
    nsums [t]    = return t
    nsums (t:ts) = fmap (AppT (AppT (TupleT 2) t)) (nsums ts)
    nsums []     = [t| () |] -- only used for empty constructor, e.g. Nothing
    
    --replaceRec = namemapT (\n -> if n == nm then wrapperNm else n) -- RECURSIVE TYPE CONVERSION

{-  FOR HANDLING CONVERTING RECURSIVE TYPES
 -  IF WE DECIDE TO DO THIS IN THE FUTURE
-- traverse a Type tree, modifying names:
namemapT :: (Name -> Name) -> Type -> Type
namemapT f (ForallT bs cxt t) = (ForallT bs cxt $ namemapT f t)
namemapT f (AppT t1 t2) = AppT (namemapT f t1) (namemapT f t2)
namemapT f (SigT t k) = SigT (namemapT f t) k
namemapT f (ConT n) = ConT $ f n
namemapT f (VarT n) = VarT $ f n
namemapT _ t = t

namemapE :: (Name -> Name) -> Exp -> Exp
namemapE = undefined
-}
