{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Data.Shapely (
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

       Structural/Duck typing is relevant here:
           http://www.haskell.org/pipermail/haskell-cafe/2009-September/066733.html
       OCaml has "Structural Polymorphism"

       TODO:
       - change name to something having to do with: structure, shape, ...
            - shapely-data, Data.Shapely to/fromShapely OR shapely/project ShapelyFoo shapelyFoo
            - structural typing: data-structural, from/toStructured 
            - or call it 'shapely' and make names and descriptions refer to "structured"
                structured/destructured
       - hlint
       - make TH code handle a list of types
       - comments, initial cabal file
       - release 0.0

       - handle empty bottom types in some way
       - some clever way to handle recursive types would be great so that we can convert [a] to (List a)
            - make fromShapely take a constructor as an argument?
            - replace recursive args with `Recursive (Foo a)`?
       - some mechanism for converting between sum types with identical but re-ordered args
            - perhaps some kind of canonical ordering of constructors
       - add infix :+ :* type operators?
       - re-write rules from/to = id, etc.
 -}


-- TODO HERE: handle derived classes, decide about strict/NotStrict, records, make
--            variable names prettier

-- | Generate a 'Shapely' instance for the referenced types. E.g.
--
-- > $(mkShapely ''Foo)  -- '' references a TH "Name"
--
-- This requires the @TemplateHaskell@ extension to be enabled.
mkShapely :: Name -> Q [Dec]
mkShapely n = 
    do (TyConI d) <- reify n  -- what about PrimTyconI? should we represent literals as newtype-wrapped literals?
       let (DataD cxt nm bndings cnstrctrs derivng) = d

       -- --------------------------------------------------------
       -- create the newtype wrapper for "shapely" version
       let wrapperNm = mkName ("Shapely" ++ nameBase nm)  -- e.g. "ShapelyFoo"
       shapelyed <- shapelyProds (nm,wrapperNm) cnstrctrs
       let unwrapperName = mkName ("shapely" ++ nameBase nm)
           wrapper = NewtypeD cxt wrapperNm bndings (RecC wrapperNm [(unwrapperName,NotStrict,shapelyed)]) [] 

       -- --------------------------------------------------------
       -- build the Shapely class instance for this type
       frmClauses <- fromShapelyClauses cnstrctrs
       --let fromShapelyDec = FunD 'fromShapely frmClauses
       let fromShapelyDec = FunD 'fromShapely $ 
             [Clause [VarP $ mkName "a"] (NormalB (AppE (VarE $ mkName "fromShapely'") (AppE (VarE unwrapperName) (VarE $ mkName "a")))) 
                [FunD (mkName "fromShapely'") frmClauses] ]

       --let toShapelyDec = FunD 'toShapely (toShapelyClauses cnstrctrs)
       let toShapelyDec = FunD 'toShapely $ 
             [Clause [VarP $ mkName "a"] (NormalB ((ConE wrapperNm) `AppE` ((VarE $ mkName "toShapely'") `AppE` (VarE $ mkName "a")))) 
                [FunD (mkName "toShapely'") (toShapelyClauses cnstrctrs)] ]

       let shapelyInstance = InstanceD [] shapelyTs [toShapelyDec, fromShapelyDec]
           shapelyTs = (ConT ''Shapely) `AppT` (decToType d) `AppT` (decToType wrapper)
       return [wrapper,shapelyInstance]

-- this is no fun... :-(
decToType :: Dec -> Type
decToType (DataD _ nm bndings _ _)    = d2t (ConT nm) bndings
decToType (NewtypeD _ nm bndings _ _) = d2t (ConT nm) bndings
d2t t bs = foldl AppT t $ map (VarT . bndNames) bs 
bndNames (PlainTV n) = n
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
fromShapelyPats (_:cs) = ConP 'Left [sumPat] : (map wrapRightPat $ fromShapelyPats cs)
fromShapelyPats [] = error "type has no constructors"

sumPat = VarP $ mkName "sumVar"
sumExp = VarE $ mkName "sumVar"
wrapRightPat p = ConP 'Right [p] 

-- given pattern match above, return a body that de-tuples args into Con
fromShapelyBdy :: Con -> Q Body
fromShapelyBdy (NormalC nm sts) = fmap NormalB $ deTuple $ length sts
    where deTuple :: Int -> Q Exp -- function applying n-kind constructor to n-nested tuples
          deTuple 0 = return (ConE nm) -- empty constructor (e.g. Nothing) 
          -- IF WE DECIDE TO, RECURSIVE TYPE CONVERSION HAPPENS HERE:
          deTuple n = [| $(deTupleN n) $(return $ ConE nm) $(return sumExp) |] -- 'sumVar' is our bound tuple above
          deTupleN 1 = [| \constr a-> constr a |]
          deTupleN n = [| \constr (a,b)-> $(deTupleN (n-1)) (constr a) b |]


---- TOSHAPELY METHOD: ----

-- takes a list of constructors to CONVERT FROM (pattern match against)
toShapelyClauses :: [Con] -> [Clause]
-- toShapelyClauses _ = [Clause [WildP] (NormalB (VarE 'undefined)) []]
toShapelyClauses cs = 
    let pats = map toShapelyPat cs
        bdies = map NormalB $ toShapelyExps cs
    in zipWith (\p bdy-> Clause [p] bdy []) pats bdies

toShapelyPat :: Con -> Pat
toShapelyPat (NormalC n sts) = ConP n $ map VarP $ take (length sts) ordNames

ordNames = [ mkName ('s':show t) | t <- [1..]] --infinite list of names for sums

toShapelyExps :: [Con] -> [Exp]
toShapelyExps [c]    = [toShapelySumExp c]
toShapelyExps (c:cs) = AppE (ConE 'Left) (toShapelySumExp c) : map (AppE $ ConE 'Right) (toShapelyExps cs)
toShapelyExps [] = error "type has no constructors"

toShapelySumExp :: Con -> Exp
toShapelySumExp (NormalC n sts) = toShapelyTuple $ take (length sts) ordNames
    where toShapelyTuple [n] = VarE n
          toShapelyTuple (n:ns) = TupE [VarE n, toShapelyTuple ns]
          -- empty constructor is unit type:
          toShapelyTuple [] = ConE '()
-- -------------------------
-- TYPE CONVERSION HELPERS:

-- takes a list of constructors from the original type and returns a single
-- data type built using only (,), Either, and ()
-- This bit does the products, calling 'shapelysums' for each constructor
shapelyProds :: (Name,Name) -> [Con] -> Q Type
shapelyProds nms [c] = let (NormalC _ args) = c
                     in shapelysums nms args
shapelyProds nms (c:cs) = let (NormalC _ args) = c
                        in [t| Either $(shapelysums nms args) $(shapelyProds nms cs) |] 
shapelyProds nms [] = [t| () |] -- only used on constructor-less types

-- convert a constructor into singleton type values, tuples and unit:
shapelysums :: (Name,Name) -> [StrictType] -> Q Type
shapelysums (nm,wrapperNm) = nsums . map snd where
--shapelysums (nm,wrapperNm) = nsums . map (replaceRec . snd) where -- RECURSIVE TYPE CONVERSION
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
