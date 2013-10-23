{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell #-}
module Data.Shapely.TH (
    deriveShapely
  ) where

import Data.Shapely.Classes
import Data.Shapely.Normal as Sh

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Control.Monad


-- TODO:
--   - newtype wrappers
--   - recursion
--   - GADT's? forall?

-- TODO: what about application and recursion, as in
--         data Free f a = Pure a | Free (f (Free f a))
--       ? No way to write a `to` for this type without relying on Functor f


-- | Generate a 'Shapely' instance for the type or set of mutually-recursive
-- types (see below) passed as argument @nms@. This will create @length nms@
-- new instances. Used like:
--
-- > $(deriveShapely [''RedTree, ''BlackTree])  -- two single-quotes reference a TH "Name"
--
-- The algorithm used here to generate the 'Normal' instance is most easily
-- described syntactically:
--
--   - Constructors are replaced with @()@, which terminate (rather than start)
--      a product
--
--   - Product terms are composed with nested tuples, e.g. @Foo a b c ==> (a,(b,(c,())))@
--
--   - The @|@ in multiconstructor ('Coproduct') type declarations is replaced
--      with @Either@
--
--   - In the instance declaration for each of the types in @nms@, all product
--     terms present in @nms@ will be wrapped in an 'AlsoNormal' newtype,
--     giving the normal form its recursive structure. e.g. 
--
--       > data List a = Cons a              (List a)      | Empty 
--       >    ==> Either (    a, (AlsoNormal (List a),()))   () 
--
--   - newtypes wrappers are not treated as structural, allowing multiple
--      instances, e.g. with different recursive structures.
--
-- Note that a 'Product' type in the @Right@ place terminates a composed
-- 'Coproduct', while a @()@ in the @snd@ place terminates the composed terms
-- of a @Product@.
deriveShapely :: [Name] -> Q [Dec]
deriveShapely [] = return []
deriveShapely nms =
    let lnms = length nms
        nnms = take lnms $ map (take lnms) $ tails $ cycle nms
     in forM nnms $ \recChildNms@(n:_) -> do
          i <- reify n 
          case i of -- :: (type family instance, toSHapely lambda, fromShapely lambda?
               (TyConI d) -> return $
                 case d of
                      (DataD _ nm bndings cnstrctrs _) -> drvShapely recChildNms Nothing d
                      (NewtypeD _ nm bindings cnstrctr _) -> error "TODO handle newtype wrappers"
                                  {-
                                                              -- GOAL: get from this mess the Dec for the type wrapped, get a name and do 'reify'?
                                                              -- remember: tupleDataName :: Int -> Name
                        -- consider:
                        --   newtype Y = Y X
                        --   newtype X = X Y
                            case cnstrctr of
                                 NormalC c_nm [(_,t)]
                                 RecC c_nm [(_,_,t)]
                                 _ -> error "Infix or forall or somehow multiple args to newtype constructor"
                                 -}
                      _ -> error "This is either impossible, or a user error"
               (PrimTyConI _ _ _) -> error "We can't generate instances for primitive type constructors. Note that 'newtype' wrapped primitive types are also not allowed, as we don't consider newtypes structural"
               _ -> error "Please pass the name of a type constructor"


drvShapely :: [Name] -> Maybe Dec -> Dec -> Dec
drvShapely recChildNms mNewtypeWrap dec@(DataD _ nm bndings cnstrctrs _) = 
    InstanceD [] (AppT (ConT ''Shapely) ( t )) [
         TySynInstD ''Normal [ t ] (

            tNorm bcnstrctrs
            -- AppT (AppT (ConT 'Either) (TupleT 0)) 
            --      (AppT (AppT (TupleT 2) (VarT a_0)) 
            --            (AppT (AppT (TupleT 2) (AppT (ConT 'AlsoNormal) 
            --                                         (AppT ListT (VarT a_0)))) (TupleT 0)))
            -- )
            )
       , FunD 'to (
              toClauses id bcnstrctrs
              -- Clause [ConP GHC.Types.[] []] (NormalB (
              --       AppE (ConE Data.Either.Left) (ConE GHC.Tuple.())
              --  )) []
            -- , Clause [ConP GHC.Types.: [VarP a_1,VarP as_2]] (NormalB (
              --       AppE (ConE Data.Either.Right) 
              --          (TupE [VarE a_1,TupE [
              --               InfixE (Just (ConE Also)) (VarE GHC.Base.$) (Just (AppE (VarE to) (VarE as_2)))
              --              ,ConE GHC.Tuple.()]]
              --          )
              --  )) []
            )
         -- i.e. constructorsOf = \_->  (the type's constructor(s))
       , ValD (VarP 'constructorsOf) (NormalB $ LamE [WildP] ( constrsOf bcnstrctrs)) []
       ]
  where
    bcnstrctrs :: [BasicCon]
    bcnstrctrs = map basicCon cnstrctrs
    
    t :: Type
    t = maybe (typedec2Type dec) typedec2Type mNewtypeWrap

    -- ----
    tNorm :: [BasicCon] -> Type
    tNorm [c] = tNormProd c
    tNorm (c:cs) = AppT (AppT (ConT ''Either) (tNormProd c)) (tNorm cs) -- i.e. Either (tNormProd c) (tNorm cs)
    tNorm _ = error "Type has no constructors, so has no 'shape'."

    tNormProd :: BasicCon -> Type
    tNormProd = tNormProd' . snd where
        tNormProd' = foldr (AppT . AppT (TupleT 2)) (TupleT 0) -- i.e. foldr (,) () constructors

    -- ----
    toClauses coprodWrapper [c] = [toClauseProd coprodWrapper c]
    toClauses coprodWrapper (c:cs) = 
      toClauseProd (coprodWrapper . AppE (ConE 'Left)) c 
        : toClauses (coprodWrapper . AppE (ConE 'Right)) cs
    toClauses coprodWrapper _ = error "Type has no constructors, so has no 'shape'."

    toClauseProd :: (Exp -> Exp) -> BasicCon -> Clause
    toClauseProd coprodWrapper (n, ts) = 
      Clause [ConP n boundVars] (NormalB $ coprodWrapper prodBody) [] -- e.g. to { (Fook a b) = Left (a,(b,())) }
        where boundNames = map (mkName . ("a"++) . show) $ map fst $ zip [0..] ts 
              boundVars :: [Pat]
              boundVars = map VarP boundNames   -- e.g. to (Fook { a0 a1 }) = ...
              prodBody :: Exp
              prodBody = tupleList $ map VarE boundNames

    -- ----
    constrsOf :: [BasicCon] -> Exp
    constrsOf [] = error "Type has no constructors, so has no 'shape'."
    constrsOf [(n,_)] = ConE n                      -- e.g. Foo :: *, or Foo :: * -> *, etc.
    constrsOf cs      = tupleList $ map (ConE . fst) cs -- e.g. (Foo, (Bar, (Baz,())))

tupleList :: [Exp] -> Exp
tupleList = foldr tupleUp (ConE '())
    where tupleUp e0 e1 = TupE [e0,e1]

type BasicCon = (Name, [Type])
basicCon :: Con -> BasicCon
basicCon (NormalC n sts)          = (n, map snd sts)
basicCon (RecC n vsts)            = (n, map (\(_,_,t)-> t) vsts)
basicCon (InfixC (_,t0) n (_,t1)) = (n, [t0,t1])
basicCon (ForallC bnds cxt con) = error "forall not handled yet" -- not sure how/if this would work


typedec2Type :: Dec -> Type
typedec2Type (DataD _ nm bndings _ _) = foldl (\c-> AppT c . VarT . varName) (ConT nm) bndings  -- i.e. data Foo a b = .. ->  .. :: Foo a b
typedec2Type (NewtypeD _ nm bndings _ _) = foldl (\c-> AppT c . VarT . varName) (ConT nm) bndings
typedec2Type _ = error "Some non-type Dec value"

varName :: TyVarBndr -> Name
varName (PlainTV n) = n
varName (KindedTV n _) = n
{-
instance Shapely [a] where 
    -- NOTE: data [] a = [] | a : [a]    -- Defined in `GHC.Types'
    type Normal [a] = Either () (a,(AlsoNormal [a],()))
    to []         = Left ()
    to ((:) a as) = Right (a, (Also $ to as, ()))
    constructorsOf _ = ([],(\a as-> (:) a (from $ normal as),()))

---->

InstanceD [] (AppT (ConT Shapely) (AppT ListT (VarT a_0))) [
    TySynInstD Normal [AppT ListT (VarT a_0)] (
        AppT (AppT (ConT Data.Either.Either) (TupleT 0)) 
             (AppT (AppT (TupleT 2) (VarT a_0)) 
                   (AppT (AppT (TupleT 2) (AppT (ConT AlsoNormal) 
                                                (AppT ListT (VarT a_0)))) (TupleT 0))))
    ,FunD to [
        Clause [ConP GHC.Types.[] []] (NormalB (AppE (ConE Data.Either.Left) (ConE GHC.Tuple.()))) []
        ,Clause [ConP GHC.Types.: [VarP a_1,VarP as_2]] (NormalB (AppE (ConE Data.Either.Right) 
                                                                       (TupE [VarE a_1,TupE [
                                                                                InfixE (Just (ConE Also)) (VarE GHC.Base.$) (Just (AppE (VarE to) (VarE as_2)))
                                                                               ,ConE GHC.Tuple.()]]
                                                                        ))) []
                                   ]
       ,ValD (VarP constructorsOf) (NormalB (VarE GHC.Err.undefined)) []
       ]
    -}
