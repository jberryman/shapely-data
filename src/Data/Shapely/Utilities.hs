module Data.Shapely.Utilities
    where

import Data.Shapely.Classes

-- | Apply a function on the 'Normal' representation of a type to an ordinary
-- value.
--
-- > ($$) f = to . f . from
infixr 0 $$
($$) :: (Shapely a, Shapely b)=> (Normal a -> Normal b) -> a -> b
($$) f = to . f . from

