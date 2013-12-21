module Data.Shapely.Utilities
    where

import Data.Shapely.Classes

-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = from . f . to
infixr 0 $$
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = from . f . to

