{-# LANGUAGE 
    MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
  , TypeFamilies
  , UndecidableInstances 
  , OverlappingInstances
  , FlexibleContexts
  , ScopedTypeVariables      -- for "advanced overlap" solution
  , EmptyDataDecls 
  , DataKinds  -- for True/False
  #-}
module Data.Shapely.Normal.Massageable
    where

import Data.Shapely.Classes
import Data.Shapely.Normal.Classes
import Data.Shapely.Normal.TypeIndexed hiding(viewType)
import Data.Shapely.Bool
import Data.Shapely.Category(swapFront)
import Data.Shapely.Utilities

import Control.Arrow((***))
import Data.Proxy

-- An internal module mostly to keep use of OverlappingInstances isolated

--TODO:
--   - support more complex recursive structure, maybe requiring user to pass a
--      list of corresponding recursive pairs in source and target


-- We need to be able to choose instances based on *whether* a type is a member
-- of a class, e.g. if the source is massageable to the left of the target
-- sum we do that, otherwise recursing to the Right. In order to do that
-- we have to turn the classes we would normally write into *predicates*, i.e.
-- where no instance would exist above, we now need to define an instance that
-- unifies a boolean head type variable to 'False'; we then chain these using
-- boolean algebra in our constraints.
--
-- technique adapted from: 
--     http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap


-- We avoid code duplication (and lots of programming errors) here & below by
-- combining the "functional" class with the predicate class. True instances
-- have real working method instances, and False instances never make it past
-- the typechecker.
class TypeIndexPred a l l' (b::Bool) | a l -> l', a l l' -> b where
    -- pull the only value of type 'a' out of 'l' yielding 'l''
    viewType :: l -> (a,l')
    viewType = error "viewType: Method called in False predicate instance"

instance (HasAny a l lHasA, Not lHasA ~ b)=> TypeIndexPred a (a,l) l b where
    viewType = id

instance (TypeIndexPred a l l' b, xl' ~ (x,l'))=> TypeIndexPred a (x,l) xl' b where
    viewType = swapFront . fmap viewType

------------ NON-INSTANCES: ------------
-- This is ugly & hackish:
data Void
instance (False ~ false, Void ~ void)=> TypeIndexPred a () void false
instance HasAny a Void False


class IsAllUnique x (b::Bool) | x -> b
instance (true ~ True)=> IsAllUnique () true
instance (IsAllUnique xs tailUnique
         , HasAny x xs xInXs
         , And tailUnique (Not xInXs) ~ b
         )=> IsAllUnique (x,xs) b


---------- wrapper classes we export: ----------

-- a flag for 'MassageableNormalRec' indicating we should not recurse into
-- subterms. No need to export
data FLAT = FLAT


-- | A class for massaging 'Normal' representation types. This works as
-- described in 'Massageable', except that it doesn't recurse into subterms. 
class MassageableNormal x y where
    -- | Convert a 'Normal' type @x@ into some 'Massageable' normal-form type @y@
    massageNormal :: x -> y

instance (MassageableNormalRec FLAT FLAT x y)=> MassageableNormal x y where
    massageNormal = massageNormalRec (Proxy :: Proxy FLAT, Proxy :: Proxy FLAT)

-- | /DISCLAIMER: this function is experimental (although it should be correct) and the behavior may change in the next version, based on user feedback. Please see list of limitations below and send feedback if you have any./
--
-- A class for typed, principled, \"fuzzy coercions\" between types.  See also
-- 'MassageableNormal'.
--
-- This works as follows (or see examples below):
--
--   - All 'Product's in the source @a@ must be mappable unambiguously to
--     exactly one product in the target @b@, according to the rules below.
--     This is a total function, and all product terms in the source are
--     preserved.
--
--   - Products in @a@ come in two flavors which are mapped differently onto
--     @b@: if a source product contains all uniquely-typed terms we treat it
--     as a type-indexed product (TIP) and its terms will be freely shuffled to
--     match its target; otherwise we consider the ordering of product terms to
--     be significant and require a target product with the same ordering. The
--     mapping between terms in source and target products is a bijection.
--
--   - We map source product subterm @a@s with target @b@ subterms, by
--     recursively applying 'massage' (this is the only exception to the above,
--     and the only place where we inspect 'Product' subterms).
--
--   - When the source @a@ is a 'Sum' this conversion may be surjective
--     w/r/t the product mappings, i.e. multiple source \"constructors\" may map
--     to the same target constructor.  But again the individual mappings must be
--     unambiguous.
--
-- Here are some examples:
--
-- > data Tsil a = Snoc (Tsil a) a | Lin
-- >           deriving Eq
-- > deriveShapely ''Tsil
-- > truth = massage "123" == Snoc (Snoc (Snoc Lin '3') '2') '1'
--
-- One limitation is we don't support a way to handle recursive structures
-- beyond top-level direct recursion (e.g. mutually-recusrive pairs of types).
-- And unlike 'coerce' functor type-applied recursive terms are not supported.
--
-- Any feedback on the above behavior would be greatly appreciated.
class Massageable a b where
    massage :: a -> b

instance (Shapely a, Shapely b
         , MassageableNormalRec a b (Normal a) (Normal b)
         )=> Massageable a b where
    massage a = let b = massageNormalRec (return a, return b) $$ a
                 in b


---------- implementation, left unexported: ----------


class MassageableNormalRec pa pb na nb where
    -- keep method hidden:
    massageNormalRec :: (Proxy pa, Proxy pb)  -- proxies for 'a' and 'b' to support recursion
                     -> na -> nb  -- (Normal a) is massaged to (Normal b)

instance (MassageableNormalRec a b s t, MassageableNormalRec a b ss t)=> MassageableNormalRec a b (Either s ss) t where
    massageNormalRec ab = either (massageNormalRec ab) (massageNormalRec ab)

instance ( IsAllUnique (x,xs) isTIPStyle
         , ProductToProductPred isTIPStyle a b (x,xs) xss isHeadMassageable
         , ProductToSum isHeadMassageable a b (x, xs) (Either xss yss)
    )=> MassageableNormalRec a b (x,xs) (Either xss yss) where
    massageNormalRec = massageProdCoprod (Proxy::Proxy isHeadMassageable)
instance ( IsAllUnique () isTIPStyle
         , ProductToProductPred isTIPStyle a b () xss isHeadMassageable
         , ProductToSum isHeadMassageable a b () (Either xss yss)
    )=> MassageableNormalRec a b () (Either xss yss) where
    massageNormalRec = massageProdCoprod (Proxy::Proxy isHeadMassageable)

instance ( Product xs, Product ys
         , IsAllUnique xs isTIPStyle
         -- Only "real" instances of ProductToProductPred will typecheck:
         , ProductToProductPred isTIPStyle a b xs ys True
    )=> MassageableNormalRec a b xs ys where
    massageNormalRec ab = massageProdProd (Proxy::Proxy isTIPStyle, ab)

alsoMassage :: (Shapely pa, Shapely pb
            )=> MassageableNormalRec pa pb (Normal pa) (Normal pb)=> (Proxy pa,Proxy pb) -> pa -> pb
alsoMassage ab a = massageNormalRec ab $$ a


------------------------------------------------------------------------------
-- MASSAGING PRODUCTS TO COPRODUCTS

class ProductToSum (isHeadMassageable::Bool) pa pb s t where
    massageProdCoprod :: Proxy isHeadMassageable -> (Proxy pa, Proxy pb) -> s -> t

instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs True
         -- insist unambiguous, else fail typechecking:
         , AnyMassageable pa pb xss ys False
         )=> ProductToSum True pa pb xss (Either xs ys) where
    massageProdCoprod _ ab = Left . massageProdProd (Proxy :: Proxy isTIPStyle,ab)

instance (MassageableNormalRec pa pb yss ys)=> ProductToSum False pa pb yss (Either xs ys) where
    massageProdCoprod _ ab = Right . massageNormalRec ab

-- helper predicate class:
class AnyMassageable pa pb xss yss (b :: Bool) | pa pb xss yss -> b
instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs headMassageable
         , AnyMassageable pa pb xss ys anyTailMassageable
         , Or headMassageable anyTailMassageable ~ b
         )=> AnyMassageable pa pb xss (Either xs ys) b
instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs b
         )=> AnyMassageable pa pb xss xs b


------------------------------------------------------------------------------
-- MASSAGING PRODUCTS TO PRODUCTS

-- combination Predicate/functional class. "False" instances are defined where we
-- would normally have not defined an instance.
class ProductToProductPred (isTIPStyle::Bool) pa pb s t (b::Bool) | isTIPStyle pa pb s t -> b where
    massageProdProd :: (Proxy isTIPStyle, (Proxy pa, Proxy pb)) -> s -> t
    massageProdProd = error "massageProdProd: Method called in False predicate instance"

------------ INSTANCES: ------------

instance ProductToProductPred either pa pb () () True where
    massageProdProd _ = id


---- TIP-style, where all source product terms are unique ----

instance ( ProductToProductPred True pa pb xxs' ys tailsTIPMassageable
         , TypeIndexPred y (x,xs) xxs' xxsHasY
         , And tailsTIPMassageable xxsHasY ~ b
    )=> ProductToProductPred True pa pb (x,xs) (y,ys) b where
    massageProdProd ps = fmap (massageProdProd ps) . viewType

-- when the head of target is a recursive 'b' term, we try to pull an
-- equivalent recursive 'a' term out of the source tuple, insisting that they also
-- be massageable. Inductive proof?
instance ( ProductToProductPred True pa pb xxs' ys tailsTIPMassageable
         , TypeIndexPred pa (x,xs) xxs' xxsHasRecursiveA
         , And tailsTIPMassageable xxsHasRecursiveA ~ b
         , MassageableNormalRec pa pb (Normal pa) (Normal pb)
         , Shapely pa, Shapely pb
    )=> ProductToProductPred True pa pb (x,xs) (pb, ys) b where
    massageProdProd ps = (alsoMassage (snd ps) *** massageProdProd ps) . viewType 


---- order-preserving style: ----

-- when massaging ordered tuples we can simply match equivalent recursive
-- massageable terms on each fst position:
instance ( ProductToProductPred False pa pb ys ys' b
         , MassageableNormalRec pa pb (Normal pa) (Normal pb)
         , Shapely pa, Shapely pb
    )=> ProductToProductPred False pa pb (pa, ys) (pb, ys') b where
    massageProdProd ps = alsoMassage (snd ps) *** massageProdProd ps

instance (ProductToProductPred False pa pb ys ys' b
    )=> ProductToProductPred False pa pb (x,ys) (x,ys') b where
    massageProdProd = fmap . massageProdProd
-- TO AVOID AMBIGUITY WITH OVERLAP:
instance (ProductToProductPred False x x ys ys' b
    )=> ProductToProductPred False x x (x,ys) (x,ys') b where
    massageProdProd = fmap . massageProdProd


------------ NON-INSTANCES: ------------

-- DEFAULT CASE:
instance (False ~ false)=> ProductToProductPred either pa pb s t false
