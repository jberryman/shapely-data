{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}  -- for "advanced overlap" solution
module Data.Shapely.Compose.Massageable
    where

import Data.Shapely.Compose.Classes

-- An internal module mostly to keep use of OverlappingInstances isolated


-- We borrow this type-equality comparison trick from Oleg: 
--   http://okmij.org/ftp/Haskell/ConEQ.hs
data Yes
data No

class HasAny a l b | a l -> b

instance HasAny a (a,l) Yes
instance (HasAny a l b)=> HasAny a (x,l) b
instance HasAny a () No

-- for 'Coproduct's:
instance HasAny p (Either p ps) Yes
instance (HasAny a (Tail (Either x l)) b)=> HasAny a (Either x l) b
instance HasAny p (Only p) Yes
instance (b ~ No)=> HasAny p (Only x) b

-- | The non-empty, type-indexed product @l@, out of which we can pull the unique type @a@, leaving @l'@
class TIP a l l' | a l -> l' where
    viewType :: l -> (a,l')

instance (HasAny a l No)=> TIP a (a,l) l where
    viewType = id

instance (TIP a l l', (x,l') ~ xl')=> TIP a (x,l) xl' where
  --viewType = swapFront . fmap viewType
    viewType (x,l) = let (a,l') = viewType l
                      in (a,(x,l'))



-- | A class for typed, hopefully-principled, \"fuzzy coercions\" between types
-- in 'Normal' form. This works as follows (or see examples below):
--
--   - when the target type @t@ is a 'Product', the terms of the source
--     product(s) are re-arranged to match the target. This must be an
--     unambiguous bijection to typecheck, so all types in the target must be
--     unique i.e. a \"type-indexed product\" (TIP)
--
--   - when @t@ is a 'Coproduct', the ordering of product terms in @s@ (whether
--     @s@ is a 'Product' or sum of products) is retained, i.e. is considered
--     to be significant, and no shuffling of these terms is done.
--
--   - When the source @s@ is a 'Coproduct' this conversion may be surjective,
--     i.e. multiple input \"constructors\" may map to the same output coproduct
--     position, but this mapping has to be unambiguous.
--
-- This behavior is partially out of necessity, but I think makes sense if you
-- believe the following:
--
--   - records aren't appropriate for sum types since they're partial
--   - if you're using record type you should use records as your only interface to the type
--
-- ...and you want this function to never do something you're not expecting.
-- I've tried to strike a good balance and accomodate both styles of treating
-- data types.
--
-- Here are some examples:
--
--    TODO put code we used in tests here !!!
--
class Massageable s t where
    massageNormal :: s -> t

instance Massageable () () where
    massageNormal = id

instance (Massageable s' l, TIP a (x,y) s')=> Massageable (x,y) (a,l) where
    massageNormal = fmap massageNormal . viewType

-- this will treat source as sum of TIPs when target is a 'Product', and drop
-- to the MassageableToCoproduct instance if a 'Coproduct'
instance (Massageable s t, Massageable ss t)=> Massageable (Either s ss) t where
    massageNormal = either massageNormal massageNormal

-- instance (MassageableToCoproduct (x,y) (Either t ts))=> Massageable (x,y) (Either t ts) where
instance (MassageableToCoproduct x (Either t ts))=> Massageable x (Either t ts) where --TODO if this doesn't work, we need separate () and (,) instances
    -- Drop into a 'massage' that observers product ordering, for when we
    -- hit the base case (x,y) (x',y'):
    massageNormal = massageNormalToCoproduct

-------
-- after: http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap

{-
class MassageableToCoproduct s t where
    massageNormalToCoproduct :: s -> t

-- TODO rename "b" to "headMassageable"
-- this defines the different instance options we can "select":
class MassageableToCoproduct' s t b where
    massageNormalToCoproduct' :: b -> s -> t

instance (MassageableToCoproduct' s t b, MtC s t b)=> MassageableToCoproduct s t where
    massageNormalToCoproduct = massageNormalToCoproduct' (undefined :: b)

-- This is the instance "selector"
class MtC s t b | s t -> b where  -- < should be renamed MassageableToCoproductEventually
instance (Massageable xxs xsx)=> MtC xxs (Either xsx ys) Yes 
instance (Massageable xxs (x,xs))=> MtC xxs (x,xs) Yes 
instance MtC () () Yes 
-- TODO options are: Yes, the head is massageable (but the tail must not be)
--                   No, (but the tail is massageable)
-- i.e. all instances correspond to "is Massageable coproduct"... that work?
instance (no ~ No)=> MtC xxs aas no

-- these constraints might not be necessary:
instance ( Massageable xxs xsx
         --, MtC xxs ys No  -- TODO: figure out how to make this work
         )=> MassageableToCoproduct' xxs (Either xsx ys) Yes where
    massageNormalToCoproduct' _ = Left . massageNormal

instance ( Massageable xxs (x,xs)
         )=> MassageableToCoproduct' xxs (x,xs) Yes where
    massageNormalToCoproduct' _ = massageNormal 
instance MassageableToCoproduct' () () Yes where
    massageNormalToCoproduct' _ = id

-- else recurse...
instance ( MassageableToCoproduct xxs ys
         )=> MassageableToCoproduct' xxs (Either as ys) No where
    massageNormalToCoproduct' _ = Right . massageNormalToCoproduct
-}
--- ------

class HeadMassageable s t b | s t -> b
instance HeadMassageable () () Yes
instance (No ~ no)=> HeadMassageable (x,y) () no
instance (No ~ no)=> HeadMassageable () (x,y) no
-- TODO when we get to (), then s' is unknown (impossible), should be Void...?
instance (And b0 b1 b, HeadMassageable s' l b0, TIPable a (x,y) s' b1)=> HeadMassageable (x,y) (a,l) b
instance (HeadMassageable xxs as b)=> HeadMassageable xxs (Either as bs) b
--TODO: do the boolean constraints have to be type funcs?
--TODO start with getting this class right first, then build

class TIPable a l l' b | a l -> l', a l l' -> b --TODO fundeps correct here?
-- Yes, but only if tail has no 'a':
instance (HasAny a l b0, Not b0 b)=> TIPable a (a,l) l b
-- recurse, looking in tail for 'a':
instance (TIPable a l l' b, (x,l') ~ xl')=> TIPable a (x,l) xl' b
-- TODO or is there a simpler but compatble way we can prove the tuple is
--      convertible, since we don't need a method here?
instance (No ~ no)=> TIPable a () x no

class And a b c | a b -> c
instance And Yes b b
instance And No  b No

class Not b b' | b -> b'
instance Not Yes No
instance Not No  Yes

{- BROKEN QUERIES:
*Data.Shapely.Compose.Massageable> let b = ('a',("hi",()))
*Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,(Char,())) (String,(Char,())) -- should not typecheck

*Data.Shapely.Compose.Massageable> massageNormal b :: Either (Int,(Char,())) (String,(Char,()))

First try to pull an Int out of (Char,(String,()))...

    *Data.Shapely.Compose.Massageable> massageNormal () :: Either () (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal () :: Either (String,(Char,())) ()
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (Char,(Int,())) (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,(Int,())) (String,(Char,()))
    *Data.Shapely.Compose.Massageable> massageNormal b :: Either (String,()) (String,(Char,()))
 -}

-- this defines the different instance options we can "select":
class MassageableToCoproduct' headMassageable s t where
    massageNormalToCoproduct' :: headMassageable -> s -> t

instance (Massageable xxs xsx
            )=> MassageableToCoproduct' Yes xxs (Either xsx ys) where -- TODO: enforce that tail is NOT massageable
    massageNormalToCoproduct' _ = Left . massageNormal

instance (Massageable xxs (x,xs)
            )=> MassageableToCoproduct' Yes xxs (x,xs) where
    massageNormalToCoproduct' _ = massageNormal

instance MassageableToCoproduct' Yes () () where -- TODO or combinable with above??
    massageNormalToCoproduct' _ = id

instance (MassageableToCoproduct xxs ys
            )=> MassageableToCoproduct' No xxs (Either xsx ys) where
    massageNormalToCoproduct' _ = Right . massageNormalToCoproduct -- N.B. not massageNormalToCoproduct'


-- our single instance class:
class MassageableToCoproduct s t where
    massageNormalToCoproduct :: s -> t

instance (MassageableToCoproduct' b s t, HeadMassageable s t b)=> MassageableToCoproduct s t where
    massageNormalToCoproduct = massageNormalToCoproduct' (undefined :: b)


 -- ------------------------
-- This is quite ugly:
{-
-- HELPER. method must be unexported.
class MassageableToCoproduct p ts b | p ts -> b where
    massageNormalToCoproduct :: p -> ts

instance ( Massageable xxs xsx
         , MassageableToCoproduct xxs ys No
         )=> MassageableToCoproduct xxs (Either xsx ys) Yes where
    massageNormalToCoproduct = Left . massageNormal
{-
instance ( Massageable (x,xs) xsx
         , MassageableToCoproduct (x,xs) ys No
         )=> MassageableToCoproduct (x,xs) (Either xsx ys) Yes where
    massageNormalToCoproduct = Left . massageNormal

instance MassageableToCoproduct () (Either () ys) Yes where
    massageNormalToCoproduct = Left . id
-}
instance ( MassageableToCoproduct xxs ys b
         )=> MassageableToCoproduct xxs (Either as ys) b where
    massageNormalToCoproduct = Right . massageNormalToCoproduct

instance ( Massageable xxs (x,xs)
         )=> MassageableToCoproduct xxs (x,xs) Yes where
    massageNormalToCoproduct = massageNormal 

instance MassageableToCoproduct () () Yes where
    massageNormalToCoproduct = id

instance (no ~ No)=> MassageableToCoproduct xxs aas no where
    massageNormalToCoproduct = error "shit"
    -}

