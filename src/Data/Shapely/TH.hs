{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Data.Shapely.TH (
    deriveShapely
  ) where

import Data.Shapely.Classes
import Language.Haskell.TH


-- TODO:
--   - GADT's? forall?
--   - support for inlining, which will be the story for:
--       data Free f a = Pure a | Free (f (Free f a))
--     ...and newtype wrappers. When the type to be inlined is a param (as
--     above), our instance must have a shapely f constraint, else we don't
--     require a Shapely instance for the type to inline; simply reify the type
--     and do all that in the background.


-- | Generate a 'Shapely' instance for the type passed as argument @nm@. Used
-- like:
--
-- > $(deriveShapely ''Tree)  -- two single-quotes reference a TH "Name" and the splice is optional
--
-- The algorithm used here to generate the 'Normal' instance is most easily
-- described syntactically:
--
--   - Constructors are replaced with @()@, which terminate (rather than start)
--      a product
--
--   - Product terms are composed with nested tuples, e.g. @Foo a b c ==> (a,(b,(c,())))@
--
--   - The @|@ in multiconstructor ('Sum') type declarations is replaced
--      with @Either@, with a nesting like the above
--
-- Note that a 'Product' type in the @Right@ place terminates a composed
-- 'Sum', while a @()@ in the @snd@ place terminates the composed terms
-- of a @Product@.
deriveShapely :: Name -> Q [Dec]
deriveShapely n = do
          i <- reify n 
          case i of
               (TyConI d) -> return $ return $
                 case d of
                      (DataD _ nm bndings cnstrctrs _) -> 
                        drvShapely (mkType nm bndings) cnstrctrs

                      (NewtypeD _ nm bndings cnstrctr _) -> 
                        drvShapely (mkType nm bndings) [cnstrctr]

                      _ -> error "This is either impossible, or a user error"
               (PrimTyConI _ _ _) -> error "We can't generate instances for primitive type constructors. Note that 'newtype' wrapped primitive types are also not allowed, as we don't consider newtypes structural"
               _ -> error "Please pass the name of a type constructor"



drvShapely :: Type -> [Con] -> Dec
drvShapely t cnstrctrs = 
    InstanceD [] (AppT (ConT ''Shapely) ( t )) [
         TySynInstD ''Normal [ t ] (tNorm bcnstrctrs)

       , FunD 'from (toClauses id bcnstrctrs)

         -- i.e. constructorsOf = \_->  (the type's constructor(s))
       , ValD (VarP 'constructorsOf) (NormalB $ LamE [WildP] ( constrsOf bcnstrctrs)) []
       ]
  where
    bcnstrctrs :: [BasicCon]
    bcnstrctrs = map basicCon cnstrctrs
    
    -- ----
    tNorm :: [BasicCon] -> Type
    tNorm [c] = tNormProd c
    tNorm (c:cs) = AppT (AppT (ConT ''Either) (tNormProd c)) (tNorm cs) -- i.e. Either (tNormProd c) (tNorm cs)
    tNorm _ = error "Type has no constructors, so has no 'shape'."

    tNormProd :: BasicCon -> Type
    tNormProd = tNormProd' . snd where
        tNormProd' = foldr (AppT . AppT (TupleT 2)) (TupleT 0) -- i.e. foldr (,) () constructors

    -- ----
    toClauses sumWrapper [c] = [toClauseProd sumWrapper c]
    toClauses sumWrapper (c:cs) = 
      toClauseProd (sumWrapper . AppE (ConE 'Left)) c 
        : toClauses (sumWrapper . AppE (ConE 'Right)) cs
    toClauses _ _ = error "Type has no constructors, so has no 'shape'."

    toClauseProd :: (Exp -> Exp) -> BasicCon -> Clause
    toClauseProd sumWrapper (n, ts) = 
      Clause [ConP n boundVars] (NormalB $ sumWrapper prodBody) [] -- e.g. from { (Fook a b) = Left (a,(b,())) }
        where boundNames = map (mkName . ("a"++) . show) $ map fst $ zip [(0 :: Int)..] ts 
              boundVars :: [Pat]
              boundVars = map VarP boundNames   -- e.g. from (Fook { a0 a1 }) = ...
              prodBody :: Exp
              prodBody = tupleList $ map VarE boundNames

    -- ----
    constrsOf :: [BasicCon] -> Exp
    constrsOf [] = error "Type has no constructors, so has no 'shape'."
    constrsOf [(n,_)] = ConE n                          -- e.g. Foo :: *, or Foo :: * -> *, etc.
    constrsOf cs      = tupleList $ map (ConE . fst) cs -- e.g. (Foo, (Bar, (Baz,())))

tupleList :: [Exp] -> Exp
tupleList = foldr tupleUp (ConE '())
    where tupleUp e0 e1 = TupE [e0,e1]

type BasicCon = (Name, [Type])
basicCon :: Con -> BasicCon
basicCon (NormalC n sts)          = (n, map snd sts)
basicCon (RecC n vsts)            = (n, map (\(_,_,t)-> t) vsts)
basicCon (InfixC (_,t0) n (_,t1)) = (n, [t0,t1])
basicCon (ForallC _ _ _)          = error "forall not handled yet" -- not sure how/if this would work


mkType :: Name -> [TyVarBndr] -> Type
mkType nm = foldl (\c-> AppT c . VarT . varName) (ConT nm)  -- i.e. data Foo a b = .. ->  .. :: Foo a b

varName :: TyVarBndr -> Name
varName (PlainTV n) = n
varName (KindedTV n _) = n
