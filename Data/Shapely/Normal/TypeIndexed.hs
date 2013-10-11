{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FunctionalDependencies , FlexibleContexts 
  , UndecidableInstances #-}
module Data.Shapely.Normal.TypeIndexed
    where

-- This is mostly in its oqwn module because it uses Overlapping instances.

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Bool


-- TODO - type filters
--      - nub / uniq
--      - Unique class , or predicate class (clean up what we have used in Massageable) 



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


-- | Shift the /only/ occurrence of type @a@ to the 'Head' of @l@.
viewType :: (HasAny a l' False, HavingType a l l')=> l -> (a,l')
viewType = viewFirstType

-- | 'viewType' of the same type as its second argument.
--
-- > viewingTypeOf = const . viewType
-- viewingTypeOf :: l -> a -> (a,l')
-- viewingTypeOf = const . viewType


-- | The non-empty, 'Product' or 'Coproduct' @l@, out of which we can pull the
-- unique type @a@, leaving @l'@.
class HavingType a l l' where
    viewFirstType :: l -> (a,l')

instance HavingType a (a,l) l where
    viewFirstType = id

instance (HavingType a l l', (x,l') ~ xl')=> HavingType a (x,l) xl' where
    viewFirstType = swapFront . fmap viewFirstType
