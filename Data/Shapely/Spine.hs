{-# LANGUAGE PolyKinds , MultiParamTypeClasses , FlexibleInstances , OverlappingInstances 
 , FlexibleContexts
 , TypeFamilies, FunctionalDependencies
 , ScopedTypeVariables
 , UndecidableInstances  -- for two BareType instances, which should always terminate
 , TypeOperators
 #-}
module Data.Shapely.Spine (
{- | 
Blah... ...
 -}
    -- * Constructing a type's Spine
      SpineOf(..), (:-:), (:-!)
    -- * Working with Proxys
    , module Data.Proxy
    , proxyTypeOf
    -- ** Creating Proxy values with parameters stripped
    , bareProxyOf
    , BareType()
    ) where

import Data.Proxy
import Data.Shapely.Classes


proxyTypeOf :: a -> Proxy a
proxyTypeOf _ = Proxy

-- TODO: or name "Bare" -> "Stripped"?

-- | Return a 'Proxy' value for type @tab@ with all of the parameters stripped
-- off, e.g.
--
-- > bareProxyOf (Just 7) :: Proxy Maybe
--
-- This is useful for specifying a spine type that is parameter-agnostic, where
-- functions using a spine of types mark a particular 'Product' term as part
-- of the spine when that term's type can be stripped down to the 'Proxy' type
-- specified. See 'SpineOf'.
bareProxyOf :: (BareType (Proxy tab) t)=> tab -> Proxy t
bareProxyOf = bareProxy . proxyTypeOf

-- TODO make closed type function and remove UndecidableInstances

-- NOTE: I want ptab to be tab, with a method :: Proxy tab -> ...
class BareType ptab t | ptab -> t where
    bareProxy :: ptab -> Proxy t

-- ...but then GHC sees ambiguous overlap...
instance (BareType (Proxy ta) t)=> BareType (Proxy (ta b)) t where
    bareProxy _ = bareProxy (Proxy :: Proxy ta)

-- ...which in my test case seems related to the `~` here.
-- TODO: test in GHC 7.8
instance (pt ~ Proxy t)=> BareType pt t where
    bareProxy _ = Proxy :: Proxy t

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
-- will match potential product terms accordingly.
--
-- This is used in 'coerceWith'
class SpineOf ts where
    -- | Allows for terse definition of the cluster of types that make up a
    -- type's spine. E.g.
    --
    -- > sp = spine :: (Foo ':-:' Bar ':-!' Baz)
    spine :: ts
instance (SpineOf ts, Shapely t)=> SpineOf (Proxy t, ts) where
    spine = (Proxy, spine :: ts)
instance SpineOf () where
    spine = ()
