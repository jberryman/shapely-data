{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FunctionalDependencies , FlexibleContexts 
  , OverlappingInstances , UndecidableInstances #-}
module Data.Shapely.Normal.TypeIndexed (
      HasAny(..)
    , viewType , viewTypeOf
    , HavingType(..)
    , DeleteAllType(..)
    , NubType(..)
    ) where

-- This is mostly in its oqwn module because it uses Overlapping instances.

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Bool


-- TODO 
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


-- TODO: generalize these to Coproducts with NormalConstr


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
viewType :: (HasAny a l' False, HavingType a l l')=> l -> (a,l')
viewType = viewFirstType

viewTypeOf :: (HasAny a l' False, HavingType a l l')=> l -> a -> (a,l')
viewTypeOf = const . viewType



-- | The non-empty, 'Product' or 'Coproduct' @l@, out of which we can pull the
-- unique type @a@, leaving @l'@.
class HavingType a l l' | a l -> l' where
    -- | Shift the first occurrence of type @a@ to the 'Head' of @l@.
    viewFirstType :: l -> (a,l')
    -- | 'viewFirstType' of the same type as its second argument.
    --
    -- > viewFirstTypeOf = const . viewFirstType
    viewFirstTypeOf :: l -> a -> (a,l')
    viewFirstTypeOf = const . viewFirstType

instance HavingType a (a,l) l where
    viewFirstType = id

instance (HavingType a l l', (x,l') ~ xl')=> HavingType a (x,l) xl' where
    viewFirstType = swapFront . fmap viewFirstType
