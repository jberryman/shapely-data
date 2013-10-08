{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}  -- for "advanced overlap" solution
module Data.Shapely.Normal.Massageable
    where

import Data.Shapely.Classes
import Data.Shapely.Normal.Classes

import Control.Arrow((***))

-- An internal module mostly to keep use of OverlappingInstances isolated


-- We borrow this type-equality comparison trick from Oleg: 
--   http://okmij.org/ftp/Haskell/ConEQ.hs
data Yes = Yes
data No = No

class And a b c | a b -> c
instance And Yes b b
instance And No  b No

class Or a b c | a b -> c
instance Or No  b b
instance Or Yes b Yes

class Not b b' | b -> b'
instance Not Yes No
instance Not No  Yes


class HasAny a l b | a l -> b

instance HasAny a (a,l) Yes
instance (HasAny a l b)=> HasAny a (x,l) b
instance HasAny a () No

-- for 'Coproduct's:
instance HasAny p (Either p ps) Yes
instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
instance HasAny p (Only p) Yes
instance (b ~ No)=> HasAny p (Only x) b

-- We need to be able to choose instances based on *whether* a type is a member
-- of a class, e.g. if the source is massageable to the left of the target
-- coproduct we do that, otherwise recursing to the Right. In order to do that
-- we have to turn the classes we would normally write into *predicates*, i.e.
-- where no instance would exist above, we now need to define an instance that
-- unifies a boolean head type variable to 'No'; we then chain these using
-- boolean algebra in our constraints.
--
-- technique adapted from: 
--     http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap


-- We avoid code duplication (and lots of programming errors) here & below by
-- combining the "functional" class with the predicate class. Yes instances
-- actually work, and No instances never make it past the typechecker.
class TypeIndexPred a l l' b | a l -> l', a l l' -> b where
    viewType :: l -> (a,l')
    viewType = error "viewType: Method called in No predicate instance"

instance (HasAny a l lHasA, Not lHasA b)=> TypeIndexPred a (a,l) l b where
    viewType = id

instance (TypeIndexPred a l l' b, xl' ~ (x,l'))=> TypeIndexPred a (x,l) xl' b where
  --viewType = swapFront . fmap viewType  --TODO
    viewType (x,l) = let (a,l') = viewType l
                      in (a,(x,l'))

------------ NON-INSTANCES: ------------
-- This is ugly & hackish:
data Void
instance (No ~ no, Void ~ void)=> TypeIndexPred a () void no
instance HasAny a Void No


class IsAllUnique x b | x -> b
instance (yes ~ Yes)=> IsAllUnique () yes
instance (IsAllUnique xs tailUnique
         , HasAny x xs xInXs
         , Not xInXs xNotInXs
         , And tailUnique xNotInXs b
         )=> IsAllUnique (x,xs) b


---------- wrapper classes we export: ----------

-- a flag for 'MassageableNormalRec' indicating we should not recurse into 'AlsoNormal'
-- subterms. No need to export
data FLAT = FLAT


-- | A class for massaging 'Normal' representation types. This works as
-- described in 'Massageable', except that it doesn't recurse into subterms. 
class MassageableNormal x y where
    -- | Convert a 'Normal' type @x@ into some 'Massageable' normal-form type @y@
    massageNormal :: x -> y

instance (MassageableNormalRec FLAT FLAT x y)=> MassageableNormal x y where
    massageNormal = massageNormalRec (FLAT,FLAT)

-- | /DISCLAIMER: this function is experimental (although it should be correct) and the behavior may change in the next version, based on user feedback. Please see list of limitations below and send feedback if you have any./
--
-- A class for typed, principled, \"fuzzy coercions\" between types.
-- See also 'MassageableNormal'.
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
--   - We map product subterms @'AlsoNormal' a@ with @AlsoNormal b@, by
--     recursively applying 'massage' (this is the only exception to the above,
--     and the only place where we inspect 'Product' subterms).
--
--   - When the source @a@ is a 'Coproduct' this conversion may be surjective
--     w/r/t the product mappings, i.e. multiple source \"constructors\" may map
--     to the same target constructor.  But again the individual mappings must be
--     unambiguous.
--
-- Here are some examples:
--
--    TODO put code we used in tests here !!!
--
-- One limitation is we don't support a way to handle recursive structures
-- beyond simple recursion (e.g. mutually-recusrive pairs of types).
--
-- Any feedback on the above behavior would be greatly appreciated.
class Massageable a b where
    massage :: a -> b

instance (Shapely a, Shapely b
         , MassageableNormalRec a b (Normal a) (Normal b)
         )=> Massageable a b where
    massage a = let b = massageNormalRec (undefined `asTypeOf` a, undefined `asTypeOf` b) $$ a
                    ($$) f = from . f . to -- TODO or move from Data.Shapely
                 in b
    

---------- implementation, left unexported: ----------


class MassageableNormalRec pa pb na nb where
    -- keep method hidden:
    massageNormalRec :: (pa, pb)  -- proxies for 'a' and 'b' to support recursion
                     -> na -> nb  -- (Normal a) is massaged to (Normal b)

instance (MassageableNormalRec a b s t, MassageableNormalRec a b ss t)=> MassageableNormalRec a b (Either s ss) t where
    massageNormalRec ab = either (massageNormalRec ab) (massageNormalRec ab)

instance ( IsAllUnique (x,xs) isTIPStyle
         , ProductToProductPred isTIPStyle a b (x,xs) xss isHeadMassageable
         , ProductToCoproduct isHeadMassageable a b (x, xs) (Either xss yss)
    )=> MassageableNormalRec a b (x,xs) (Either xss yss) where
    massageNormalRec = massageProdCoprod (undefined::isHeadMassageable)
instance ( IsAllUnique () isTIPStyle  -- TODO THE BUG IS HERE
         , ProductToProductPred isTIPStyle a b () xss isHeadMassageable
         , ProductToCoproduct isHeadMassageable a b () (Either xss yss)
    )=> MassageableNormalRec a b () (Either xss yss) where
    massageNormalRec = massageProdCoprod (undefined::isHeadMassageable)

instance ( Product xs, Product ys
         , IsAllUnique xs isTIPStyle
         -- Only "real" instances of ProductToProductPred will typecheck:
         , ProductToProductPred isTIPStyle a b xs ys Yes
    )=> MassageableNormalRec a b xs ys where
    massageNormalRec ab = massageProdProd (undefined::isTIPStyle, ab)

alsoMassage :: MassageableNormalRec pa pb (Normal pa) (Normal pb)=> (pa,pb) -> AlsoNormal pa -> AlsoNormal pb
alsoMassage ab = Also . massageNormalRec ab . normal


------------------------------------------------------------------------------
-- MASSAGING PRODUCTS TO COPRODUCTS

class ProductToCoproduct isHeadMassageable pa pb s t where
    massageProdCoprod :: isHeadMassageable -> (pa,pb) -> s -> t

instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs Yes
         -- insist unambiguous, else fail typechecking:
         , AnyMassageable pa pb xss ys No
         )=> ProductToCoproduct Yes pa pb xss (Either xs ys) where
    massageProdCoprod _ ab = Left . massageProdProd (undefined :: isTIPStyle,ab)

instance (MassageableNormalRec pa pb yss ys)=> ProductToCoproduct No pa pb yss (Either xs ys) where
    massageProdCoprod _ ab = Right . massageNormalRec ab

-- helper predicate class:
class AnyMassageable pa pb xss yss b | pa pb xss yss -> b
instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs headMassageable
         , AnyMassageable pa pb xss ys anyTailMassageable
         , Or headMassageable anyTailMassageable b
         )=> AnyMassageable pa pb xss (Either xs ys) b
instance ( IsAllUnique xss isTIPStyle
         , ProductToProductPred isTIPStyle pa pb xss xs b
         )=> AnyMassageable pa pb xss xs b


------------------------------------------------------------------------------
-- MASSAGING PRODUCTS TO PRODUCTS

-- combination Predicate/functional class. "No" instances are defined where we
-- would normally have not defined an instance.
class ProductToProductPred isTIPStyle pa pb s t b | isTIPStyle pa pb s t -> b where
    massageProdProd :: (isTIPStyle, (pa,pb)) -> s -> t
    massageProdProd = error "massageProdProd: Method called in No predicate instance"

------------ INSTANCES: ------------

instance ProductToProductPred either pa pb () () Yes where
    massageProdProd _ = id


---- TIP-style, where all source product terms are unique ----

instance ( ProductToProductPred Yes pa pb xxs' ys tailsTIPMassageable
         , TypeIndexPred y (x,xs) xxs' xxsHasY
         , And tailsTIPMassageable xxsHasY b
    )=> ProductToProductPred Yes pa pb (x,xs) (y,ys) b where
    massageProdProd ps = fmap (massageProdProd ps) . viewType

-- when the head of target is a recursive 'b' term, we try to pull an
-- equivalent recursive 'a' term out of the source tuple, insisting that they also
-- be massageable.
instance ( ProductToProductPred Yes pa pb xxs' ys tailsTIPMassageable
         , TypeIndexPred (AlsoNormal pa) (x,xs) xxs' xxsHasRecursiveA
         , And tailsTIPMassageable xxsHasRecursiveA b
         , MassageableNormalRec pa pb (Normal pa) (Normal pb)
    )=> ProductToProductPred Yes pa pb (x,xs) (AlsoNormal pb, ys) b where
    massageProdProd ps = (alsoMassage (snd ps) *** massageProdProd ps) . viewType -- TODO: or make a massageable instance for AlsoNormal?


---- order-preserving style: ----

instance (ProductToProductPred No pa pb ys ys' b
    )=> ProductToProductPred No pa pb (x,ys) (x,ys') b where
    massageProdProd = fmap . massageProdProd

-- when massaging ordered tuples we can simply match equivalent AlsoNormal
-- terms on each fst position:
instance ( ProductToProductPred No pa pb ys ys' b
         , MassageableNormalRec pa pb (Normal pa) (Normal pb)
    )=> ProductToProductPred No pa pb (AlsoNormal pa, ys) (AlsoNormal pb, ys') b where
    massageProdProd ps = alsoMassage (snd ps) *** massageProdProd ps


------------ NON-INSTANCES: ------------

instance (No ~ no)=> ProductToProductPred either pa pb s t no
