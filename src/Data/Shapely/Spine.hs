{-# LANGUAGE PolyKinds , MultiParamTypeClasses , FlexibleInstances
 , FlexibleContexts
 , ScopedTypeVariables
 , TypeOperators
 , UndecidableInstances  -- for (Shapely tab, Applied t tab) constraint
 #-}
module Data.Shapely.Spine (
{- | 
 -}
    -- * Constructing a type's Spine
      SpineOf(..), (:-:), (:-!)
    -- * Working with Proxys
    , module Data.Proxy
    , proxyTypeOf
    ) where

import Data.Proxy
import Data.Shapely.Classes
import Data.Proxy.Kindness


-- | > proxyTypeOf = return
proxyTypeOf :: a -> Proxy a
proxyTypeOf = return

infixr :-:
type t :-: ts = (Proxy t, ts)
infixr :-!
type t1 :-! t2 = (Proxy t1, (Proxy t2, ()))

-- | The \"spine\" of some 'Shapely' instance type can be specified by
-- enumerating the types of its recursive subterms in a 'Product' of 'Proxy'
-- values. For instance the spine of @data L = Cons Int L | Empty@ would simply
-- be
--
-- > (Proxy :: Proxy L, ())
--
-- When parameterized types make up the recursive structure, like @[a]@ one can
-- specify the spine using just the base type (in this case @[]@), e.g.
--
-- > (Proxy :: Proxy [], ())
--
-- ...or the base type applied as far as desired and functions using this spine
-- will match potential product terms accordingly (e.g. 'coerceWith').
--
-- See "proxy-kindness" for utilities useful fo constructing 'Spine's, e.g.
-- 'unappliedOf'.
class SpineOf ts where
    -- | Allows for terse definition of the cluster of types that make up a
    -- type's spine. E.g.
    --
    -- > sp = spine :: (Foo ':-:' Bar ':-!' Baz)
    spine :: ts
instance (Shapely tab, Applied t tab, SpineOf ts)=> SpineOf (Proxy t, ts) where
    spine = (Proxy, spine :: ts)
instance SpineOf () where
    spine = ()
