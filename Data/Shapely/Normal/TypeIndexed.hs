{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FunctionalDependencies , FlexibleContexts , TypeFamilies
  , OverlappingInstances , UndecidableInstances #-}
module Data.Shapely.Normal.TypeIndexed (
      HasAny(..)
    , viewType , viewTypeOf
    , HavingType(..)
    -- ** On 'Product's
    , DeleteAllType(..)
    , NubType(..)
    ) where

-- This is mostly in its oqwn module because it uses Overlapping instances.

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Bool
import Data.Shapely.Classes

-- TODO 
--    - type indexed 'factor' (and even 'distribute')
--    - inject :: prod -> coprod
--    - `nub` for coprod (perhaps calling `inject`)
--    - Unique (or TypeSet, or...?) class (i.e. for products this is a TIP), or
--    predicate class (clean up TypeIndexPred in Massageable, keeping method
--    hidden) 


-- We borrow this type-equality comparison trick from Oleg: 
--   http://okmij.org/ftp/Haskell/ConEQ.hs
class HasAny a l b | a l -> b

instance HasAny a (a,l) True
instance (HasAny a l b)=> HasAny a (x,l) b
instance HasAny a () False

-- for 'Coproduct's:
instance HasAny p (Either p ps) True
instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
instance HasAny p (Only p) True
instance (false ~ False)=> HasAny p (Only x) false



class NubType l l' | l -> l' where
    -- | Remove all but the first occurrence of each type.
    nubType :: l -> l'

instance (() ~ l')=> NubType () l' where
    nubType () = ()

instance (DeleteAllType x xys ys, NubType ys ys', x_ys' ~ (x,ys'))=> NubType (x,xys) x_ys' where
    nubType (x,xys) = (x, nubType (xys `deleteAllTypeOf` x))



class DeleteAllType a l l' | a l -> l' where
    -- | Drop any occurrences of type @a@ from the list @l@, leaving @l'@.
    deleteAllTypeOf :: l -> a -> l'

instance (u ~ ())=> DeleteAllType a () u where
    deleteAllTypeOf = const
    
instance (DeleteAllType a l l')=> DeleteAllType a (a,l) l' where
    deleteAllTypeOf = deleteAllTypeOf . snd

instance (DeleteAllType a l l', (x,l') ~ x_l')=> DeleteAllType a (x,l) x_l' where
    deleteAllTypeOf l a = fmap (`deleteAllTypeOf` a) l


-- | Shift the /only/ occurrence of type @a@ to the 'Head' of @l@.
-- 
-- > viewType = viewFirstType
viewType :: (HasAny a (Tail (NormalConstr l a l')) False, HavingType a l l')=> l -> NormalConstr l a l'
viewType = viewFirstType

viewTypeOf :: (HasAny a (Tail (NormalConstr l a l')) False, HavingType a l l')=> l -> a -> NormalConstr l a l'
viewTypeOf = const . viewType



-- | The non-empty, 'Product' or 'Coproduct' @l@, out of which we can pull the
-- first occurrence of type @a@, leaving as the 'Tail' @l'@.
class HavingType a l l' | a l -> l' where
    -- | Shift the first occurrence of type @a@ to the 'Head' of @l@.
    viewFirstType :: l -> NormalConstr l a l'
    -- | 'viewFirstType' of the same type as its second argument.
    --
    -- > viewFirstTypeOf = const . viewFirstType
    viewFirstTypeOf :: l -> a -> NormalConstr l a l'
    viewFirstTypeOf = const . viewFirstType

instance (Product l)=> HavingType a (a,l) l where
    viewFirstType = id

instance (Product l, HavingType a l l', (x,l') ~ xl')=> HavingType a (x,l) xl' where
    viewFirstType = swapFront . fmap viewFirstType


-- match Left
instance (Coproduct (Either () ps))=> HavingType () (Either () ps) ps where
    viewFirstType = id
instance (Coproduct (Either (x,y) ps))=> HavingType (x,y) (Either (x,y) ps) ps where
    viewFirstType = id
-- (ugly. needed to steal instance from the next section below):
instance (Coproduct (Either () ()))=> HavingType () (Either () ()) () where
    viewFirstType = id
instance (Coproduct (Either (x,y) (x,y)))=> HavingType (x,y) (Either (x,y) (x,y)) (x,y) where
    viewFirstType = id

-- match Right Product
instance (x' ~ x, Product p)=> HavingType p (Either x p) x' where
    viewFirstType = swap

-- recurse Right:
instance (HavingType a y l', Either x l' ~ xl', Coproduct y)=> HavingType a (Either x y) xl' where
    viewFirstType = swapFront . fmap viewFirstType
