{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Data.Shapely (
{- |
   This is an experimantal module for converting aribtrary algebraic data types
   into combinations of haskell's primitive product (@(,)@), sum (@Either@),
   and unit (@()@) types. The idea is to move the /structure/ of a data type
   into the type system. 
   .
   The templeate haskell function 'mkName' can be used in a splice to generate
   'Shapely' class instances for a list of types. Here is an example of a
   Shapely instance generated for @Maybe@, illustrating naming conventions
   in generated code:
   .
   > {-# LANGUAGE TemplateHaskell #-}
   > -- This code:
   > $(mkShapely [''Maybe])
   > -- generates code equivalent to:
   > {-
   >  newtype ShapelyMaybe a = ShapelyMaybe {shapelyMaybe :: Either () a}
   >  instance Shapely (Maybe a) (ShapelyMaybe a) where
   >        toShapely a = ShapelyMaybe (toShapely' a)
   >          where
   >              toShapely' Nothing = Left GHC.Unit.()
   >              toShapely' (Just s1) = Right s1
   >        fromShapely a = fromShapely' (shapelyMaybe a)
   >          where
   >              fromShapely' (Left sumVar) = Nothing
   >              fromShapely' (Right sumVar)
   >                = \constr a-> constr a Just sumVar 
   > -}
   .
   Note that the resulting "structural form" might be ambiguous, for instance
   both the types @data Foo = Foo Int | Empty@ and @data Bar = Bar Int |
   HoldsUnit ()@ will convert to @Either Int ()@. This poses no problem for
   conversions however.
   .
   This is mostly proof-of-concept, but some potentially-useful applications
   for this and future versions:
   .
   - generic view functions and lenses 
   - conversions between similarly-structured data, or "canonical representation"
   - incremental @Category@-level modification of data structure, e.g. with @Arrow@
   - serializing data types
   .
   /Caveats:/ In this version only basic (non-record) types are supported,
   recursive type arguments are not converted, etc. Let me know if you would
   find this module useful with additional functionality or more robust handling
   of input types.
-}
    mkShapely
  , Shapely(..)
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | A class for types to be converted into a sort of "normal form" by
-- converting its constructors into a combination of @Either@, @(,)@
-- and @()@, and back again.
class Shapely a b | a -> b, b -> a where
    toShapely   :: a -> b
    fromShapely :: b -> a


{-
 - NOTE:

       TODO:
       - release 0.0

       - generate pairs of convenience functions:
           toShapelyFoo = shapelyFoo . toShapely
           fromShapelyFoo = fromShapely . ShapelyFoo
       - support record types, type operators e.g. [], strict/non-strict, derived classes, prettify variable names, etc. etc.
       - handle empty bottom types in some way
       - some clever way to handle recursive types so that we can convert [a] to (List a)
            - make fromShapely take a constructor as an argument?
            - replace recursive args with `Recursive (Foo a)`?
       - types with equivalent shape, except for constructor ordering should be convertible back and forth
            - perhaps some kind of canonical ordering of constructors
       - add infix :+ :* type operators?
       - if this becomes more useful, the Shapely class should live in it's own module
       - re-write rules from/to = id, etc.
 -}


-- | Generate a 'Shapely' instance and newtype wrapper for the referenced
-- types (see above for naming conventions). Usage:
--
-- > $(mkShapely [''Foo])  -- single-quotes reference a TH "Name"
--
-- This requires the @TemplateHaskell@ extension to be enabled.
mkShapely :: [Name] -> Q [Dec]
mkShapely = fmap concat . mapM mkShapely' where
    mkShapely' n = do
       (TyConI d) <- reify n  -- what about PrimTyconI? should we represent literals as newtype-wrapped literals?
       let (DataD ctxt nm bndings cnstrctrs derivng) = d

       -- --------------------------------------------------------
       -- create the newtype wrapper for "shapely" version
       let wrapperNm = mkName ("Shapely" ++ nameBase nm)  -- e.g. "ShapelyFoo"
       shapelyed <- shapelyProds  cnstrctrs
       --shapelyed <- shapelyProds (nm,wrapperNm) cnstrctrs
       let unwrapperName = mkName ("shapely" ++ nameBase nm)
           wrapper = NewtypeD ctxt wrapperNm bndings (RecC wrapperNm [(unwrapperName,NotStrict,shapelyed)]) [] 

       -- --------------------------------------------------------
       -- build the Shapely class instance for this type
       frmClauses <- fromShapelyClauses cnstrctrs
       let fromShapelyDec = FunD 'fromShapely $ 
             [Clause [VarP $ mkName "a"] (NormalB (AppE (VarE $ mkName "fromShapely'") (AppE (VarE unwrapperName) (VarE $ mkName "a")))) 
                [FunD (mkName "fromShapely'") frmClauses] ]

       let toShapelyDec = FunD 'toShapely $ 
             [Clause [VarP $ mkName "a"] (NormalB ((ConE wrapperNm) `AppE` ((VarE $ mkName "toShapely'") `AppE` (VarE $ mkName "a")))) 
                [FunD (mkName "toShapely'") (toShapelyClauses cnstrctrs)] ]

       let shapelyInstance = InstanceD [] shapelyTs [toShapelyDec, fromShapelyDec]
           shapelyTs = (ConT ''Shapely) `AppT` (decToType d) `AppT` (decToType wrapper)
       return [wrapper,shapelyInstance]

-- this is no fun... :-(
decToType :: Dec -> Type
decToType (DataD _ nm bndings _ _)    = d2t nm bndings
decToType (NewtypeD _ nm bndings _ _) = d2t nm bndings
decToType _ = error "TODO, sorry"

d2t :: Name -> [TyVarBndr] -> Type
d2t nm = foldl AppT (ConT nm) . map (VarT . bndNames)
    where bndNames (PlainTV n) = n
          bndNames (KindedTV n _) = n



-- -------------------------
-- DATA CONVERSION HELPERS:

---- FROMSHAPELY METHOD: ----
-- takes list of constructors to CONVERT TO, pattern matching against Either, (), (,)
fromShapelyClauses :: [Con] -> Q [Clause]
fromShapelyClauses cs = do 
    bdies <- mapM fromShapelyBdy cs
    let pats = fromShapelyPats cs
    return $ zipWith (\p bdy-> Clause [p] bdy []) pats bdies

-- for each constructor, return the pattern to match 
fromShapelyPats :: [Con] -> [Pat]
fromShapelyPats [_] = [sumPat]
fromShapelyPats (_:cs) = ConP 'Left [sumPat] : (map (\p-> ConP 'Right [p]) $ fromShapelyPats cs)
fromShapelyPats [] = error "type has no constructors"

sumPat :: Pat
sumPat = VarP $ mkName "sumVar"
sumExp :: Exp
sumExp = VarE $ mkName "sumVar"

-- given pattern match above, return a body that de-tuples args into Con
fromShapelyBdy :: Con -> Q Body
fromShapelyBdy (NormalC nm sts) = fmap NormalB $ deTuple $ length sts
    where deTuple :: Int -> Q Exp -- function applying n-kind constructor to n-nested tuples
          deTuple 0 = return (ConE nm) -- empty constructor (e.g. Nothing) 
          -- IF WE DECIDE TO, RECURSIVE TYPE CONVERSION HAPPENS HERE:
          deTuple n = [| $(deTupleN n) $(return $ ConE nm) $(return sumExp) |] -- 'sumVar' is our bound tuple above
          deTupleN 1 = [| \constr a-> constr a |]
          deTupleN n = [| \constr (a,b)-> $(deTupleN (n-1)) (constr a) b |]
fromShapelyBdy _ = error "TODO, sorry"


---- TOSHAPELY METHOD: ----
-- takes a list of constructors to CONVERT FROM (pattern match against)
toShapelyClauses :: [Con] -> [Clause]
toShapelyClauses cs = 
    let pats = map toShapelyPat cs
        bdies = map NormalB $ toShapelyExps cs
    in zipWith (\p bdy-> Clause [p] bdy []) pats bdies

toShapelyPat :: Con -> Pat
toShapelyPat (NormalC n sts) = ConP n $ map VarP $ take (length sts) ordNames
toShapelyPat _ = error "TODO, sorry"

ordNames :: [Name]
ordNames = [ mkName ('s':show t) | t <- [1..] :: [Int] ] --infinite list of names for sums

toShapelyExps :: [Con] -> [Exp]
toShapelyExps [c]    = [toShapelySumExp c]
toShapelyExps (c:cs) = AppE (ConE 'Left) (toShapelySumExp c) : map (AppE $ ConE 'Right) (toShapelyExps cs)
toShapelyExps [] = error "type has no constructors"

toShapelySumExp :: Con -> Exp
toShapelySumExp (NormalC _ sts) = toShapelyTuple $ take (length sts) ordNames
    where toShapelyTuple [n] = VarE n
          toShapelyTuple (n:ns) = TupE [VarE n, toShapelyTuple ns]
          -- empty constructor is unit type:
          toShapelyTuple [] = ConE '()
toShapelySumExp _ = error "TODO, sorry"


-- -------------------------
-- TYPE CONVERSION HELPERS:

-- takes a list of constructors from the original type and returns a single
-- data type built using only (,), Either, and ()
-- This bit does the products, calling 'shapelysums' for each constructor
shapelyProds :: [Con] -> Q Type
shapelyProds [NormalC _ args] = shapelysums args
shapelyProds ((NormalC _ args):cs) = 
    [t| Either $(shapelysums args) $(shapelyProds cs) |] 
shapelyProds [] = [t| () |] -- only used on constructor-less types
shapelyProds _ = error "TODO, sorry"

-- convert a constructor into singleton type values, tuples and unit:
shapelysums :: [StrictType] -> Q Type
shapelysums = nsums . map snd where
--shapelysums (nm,wrapperNm) = nsums . map (replaceRec . snd) where -- RECURSIVE TYPE CONVERSION
    nsums [t]    = return t
    nsums (t:ts) = fmap (AppT (AppT (TupleT 2) t)) (nsums ts)
    nsums []     = [t| () |] -- only used for empty constructor, e.g. Nothing
    
    --replaceRec = namemapT (\n -> if n == nm then wrapperNm else n) -- RECURSIVE TYPE CONVERSION

{-  FOR HANDLING CONVERTING RECURSIVE TYPES
 -  IF WE DECIDE TO DO THIS IN THE FUTURE
-- traverse a Type tree, modifying names:
namemapT :: (Name -> Name) -> Type -> Type
namemapT f (ForallT bs ctxt t) = (ForallT bs ctxt $ namemapT f t)
namemapT f (AppT t1 t2) = AppT (namemapT f t1) (namemapT f t2)
namemapT f (SigT t k) = SigT (namemapT f t) k
namemapT f (ConT n) = ConT $ f n
namemapT f (VarT n) = VarT $ f n
namemapT _ t = t

namemapE :: (Name -> Name) -> Exp -> Exp
namemapE = undefined
-}
