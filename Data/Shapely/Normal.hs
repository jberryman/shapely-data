{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- for nested Type families. We intend these to be closed anyway, so no biggie
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- NO OVERLAPPING INSTANCES HERE, PLEASE
module Data.Shapely.Normal (
{- |
Functions for composing and modifying our 'Normal' form types.

These take their names from the familiar functions in Data.List and Prelude,
but are given more general forms, sometimes loosely after the constructions in
Edward Kmett's "categories" package. There are probably many improvements and
additions possible here.

/NOTE/: The structure of the classes, type functions, and class constraints
here are likely to change a lot, however the names of individual functions and
methods and their use should be stable, so hopefully there will be few
compatibility issues when this module is improved.
-}
      Only(..)

    -- * Operations on 'Product's and 'Coproduct's
    , Reversable(..)
    , Shiftable(..)
    , viewr
 -- , Appendable(..), Concatable(..)
    -- ** Convenience Type synonyms
    , (:*:), (:*!), (:+:)

    -- * Operations on Products
    , Homogeneous(..), FixedList()
    -- ** Construction Convenience Operators
    , single
    , (|>), (<|), (<!)

    -- * Product and Coproduct Conversions
    , MassageableNormal(..)

    -- * Algebraic
    -- ** Factoring
    , extract
    , FactorPrefix(..)

    -- ** Distributing
    , DistributeTerm(..), (:*<:)
    , Multiply(..) 
    
    -- ** Exponentiation
    -- | In the algebra of algebraic datatypes, @(->)@ is analogous to
    -- exponentiation, where @b^a (TODO notation) == (a -> b). Lots of useful
    -- and simple functions fall out from simply translating the algebraic laws
    -- of exponents to haskell.
    , Exponent(..), (:=>->), Base(..), (:->=>)
    , constructorsOfNormal

    -- ** Constants
    , Constant(..), Length(..), Replicated(..)
  --, _1st, _2nd, _3rd, _4th, _5th, _6th, _7th, _8th, _9th, _last --TODO or _1, _2, ... or _1th, _2th, etc
  --, ofLength (e.g. _2nd `ofLength`)
  --     but should this take a /Product/ as argument? 
  --     Answer: where are we using these constants?
  --         - indexing via exponential form
  --         - indexing into List (labeled with a Constant)
  --         - 
  --, _+_ , succ, (math with ordinal numbers; this would add the ordinal representations)
    ) where

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Classes
import Data.Shapely.Normal.Massageable
import Data.Shapely.Normal.Exponentiation
import Control.Applicative() -- Functor instances for (,) and Either

import Prelude hiding (replicate,concat,reverse, map)
import qualified Prelude 

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)

-- TODO:
--      - comment concatable/appendable
--      - look at which functions are algebraic, re-order, put under -- * Algebraic heading
--      - add missing functions
--      - consider renaming of all
--      - look over notes again, see if we missed anything
--      - fix Concatable/Appendable, add better Monoidal class
--      - add in unicode math stuff in docs.
--      - see about any '*As' variants that might be useful, for type inferrence
--      - implement FunctorOn
--      - create a length-indexed list (opaque for safety) that is, perhaps
--        Foldable/Traversable and can be converted from and back into a Product.
--        Maybe replace toList with Foldable.toList
--        add a safe index function
--
--      - look into type families w/ coincident overlap for SHapely.Bool module
--           http://typesandkinds.wordpress.com/2013/04/29/coincident-overlap-in-type-families/
--           - also try to see if / where this might improve inferrence
--      - make sure we know exactly how OverlappingInstances and
--        UndecidableInstances are working and that they're safe here
--          - double check all uses (remove / recompile / assess)
--      - derive Shapely instances for all built-in types
--      - documentation:
--          - notes about Algebraic stuff
--          - fix 'limitations' section
--          - make a note about future plans for inlining.
--      - implement thorough tests for 'massage', and TH-derived stuff.
--          - especially recursion, which we haven't tested well
--      - take last look at easy construction of normal-form types
--      - create some examples that re-create GHC generics motivation
--      - finalize exports, modules, finish cabal file w/ proper docs & motivation
--
--   v0.2:
--      - move to closed type families, look at replacing OverlappingInstances
--        with these by using families for type equality
--          - use closed type fams in proxy-kindness too
--      - use some scheme to close type classes (maybe closed type fams will help)
--      - freeze 'massage' behavior
--      - support inlining, and "templates" defining structure. Have NormalR use this
--      - maybe: 
--          - type-indexed 'factor' (and maybe 'distrivbute'), maybe two variants:
--              - automatically factors out all common terms (regardless of position)
--              - factors out specifically-requested terms (regardless of position
--          - incorporate TypeNat stuff (for specifying length and constructor number)
--
--    sometime:
--      - function that does a series expansion (up to _nth) of a recursive type
--      - read up about "row types"
--      - see if where and if we can make interesting use of DataKinds
--          - replacing didactic classes with "kindly-typed" structures?
--      - read "Small induction recursion..." paper
--      - check out "multirec" approach to recursive structure, and http://mainisusuallyafunction.blogspot.com/2010/12/type-level-fix-and-generic-folds.html
--      - consider some scheme (from proxy-kindness) for optionally "guarding"
--         ambiguous/polymorphic terms coming from parameterized types in
--         functions that make use of type equality (e.g. massage)
--
-- OTHER FUNCTIONS:
--   
--   - straighten :: e.g. ((a,b),(c,(d,e))) -> (a,(b,(c,(d,(e,()))))) -- requires closed type family (or overlapping instances)
--   - factor & distribute, (+ type-indexed variants)
--   - a recursive, monoid-style `zipWith` (i.e. an `mappend` that does something useful recursively)?
--
--   taking a numeric arg (implement with sum/product "templates"/peano numbers)
--     - length type func. (with type nats?)
--     - splitAt  (product take/drop in terms of this)
--     - popi (or factori?). product (!!) in terms of this
--     - replicate
--
--   complex, maybe not useful:
--     - intersperse
--     - intercalate
--     - transpose
--
--   products only: (USEFUL; DO LATER)
--     - zips ... e.g.:  zip ( (1,(2,())) , ( (True,(False,())) ,  ( ('a',('b'('c',()))) , ()))) == ....
--         but should we (can we) deal with differing-length prods like zip?  ... type Zipped () (a,b) = () , Zipped (a,b) () = () , Zipped (a,bs) (x,ys) = ( (a,(x,())) , Zipped bs ys) ...or for prods of prods we probably will be wanting a fold on products... oy
--         note also: above should be named "transpose"
--     Homogeneous only:
--       - and, or, sum, product, maximum, minimum, folds (NO: toList is sufficient for all of these folds)
--       - maps, scans
--       - sort/insert ?
--
-- -------
-- FUTURE TODOs:
--   -figure out rest of equivalents of categories' Control.Category.Cartesian. see notes re. replicate & sum/product numerals (templates?)
--   -closed TypeFamilies for simpler instances?
--   -type-level naturals for e.g. 'length;-like functions
--   -explore relationship to:
--     -lens
--     -vinyl
--     -Arrow
--     - "lifted" programming with -XDataKinds
-- -------




infixr 5 :+:
infixr 6 :*:
infixr 6 :*!
type (:+:) = Either
type (:*:) = (,)
type (x :*! y) = (x,(y,()))


-- TODO: use instance defaulting here for these definitions after they are settled?
-- TODO: if we combine these methods into bigger classes, can we omit some constraints and simplify things?


-- | Note: @viewl@ would be simply @id@.
--
-- > viewr = swap . shiftr
viewr :: (Symmetric (->) p, Shiftable t, ShiftedR t ~ p a b) => t -> p b a
viewr = swap . shiftr

-- TODO: or name this class "higher-order normal" or something?
--       we should be able to do 'append' here with a match on ((x,xs),xss) .. ((),xss)... etc.
--       maybe remove this; confusing since Either instance not a Coproduct.
-- | Class for flattening a 'Product' of 'Product's, or a nested sum of
-- 'Coproduct's.
class Concatable xs where
    type Concated xs
    -- | A generalization of 'Data.List.concat' for sums and products
    concat :: xs -> Concated xs

instance Concatable () where
    type Concated () = ()
    concat = id

-- | combine a product of products
instance (Concatable yss, Appendable xs (Concated yss)
         , Product xs, Product (Concated yss)
    )=> Concatable (xs,yss) where
    type Concated (xs,yss) = xs :++: Concated yss
    concat = append . fmap concat

-- These are the two possible leaf Coproducts at the Last position of
-- our Either spine:
instance Concatable (Either () es) where
    type Concated (Either () es) = (Either () es)
    concat = id

instance Concatable (Either (x,ys) es) where
    type Concated (Either (x,ys) es) = (Either (x,ys) es)
    concat = id

-- | Note here that @xs@ is not a 'Coproduct', since it is a sum of sums.
instance (Concatable ess, Appendable (Either x ys) (Concated ess)
         , Coproduct (Either x ys), Coproduct (Concated ess)
    )=> Concatable (Either (Either x ys) ess) where
    type Concated (Either (Either x ys) ess) = Either x ys :++: Concated ess
    concat = append . fmap concat


-- | Reversing 'Products' and 'Coproduct's
class Reversable t where
    type Reversed t
    type Reversed t = Reversed (Tail t) :> Head t
    reverse :: t -> Reversed t

instance Reversable () where
    type Reversed () = ()
    reverse = id

instance (Reversable xs
         , Shiftable (x, Reversed xs)
         )=> Reversable (x,xs) where
    reverse = shiftl . fmap reverse 

instance Reversable (Either x ()) where
    type Reversed (Either x ()) = Either () x
    reverse = swap
instance Reversable (Either x (a,b)) where
    type Reversed (Either x (a,b)) = Either (a,b) x
    reverse = swap

instance ( xs ~ Either y zs  -- for readability
         , Reversed (Either x xs) 
            ~ ShiftedL (Either x (Reversed xs))
         , Reversable xs
         , Shiftable (Either x (Reversed xs))
    )=> Reversable (Either x (Either y zs)) where
    reverse = shiftl . fmap reverse 


-- TODO: make instance for (), making ShiftedL/R assoc types?
-- | a class for shifting a sum or product left or right by one element, i.e. a
-- logical shift
class Shiftable t where
    shiftl :: t -> ShiftedL t
    shiftr :: t -> ShiftedR t
    
type ShiftedL t = Tail t :> Head t
type ShiftedR t = Last t :< Init t

instance Shiftable (x,()) where
    shiftl = id
    shiftr = id

instance (Shiftable (y,zs)
        , Shiftable (x, zs)
        , ShiftedR (y,zs) ~ (Last (y, zs), Init (y, zs))
        )=> Shiftable (x,(y,zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr

instance Shiftable (Either a ()) where
    shiftl = swap
    shiftr = swap
instance Shiftable (Either a (x,y)) where
    shiftl = swap
    shiftr = swap

instance (Shiftable (Either y zs)
        , Shiftable (Either x zs)
        , xs ~ (Either y zs) -- for readability
        -- TODO simplify these
        , Tail (Either x zs) ~ Tail xs
        , (Last xs :< (x :< Init xs)) ~ Either a0 (Either x c0)
        , (Last xs :< Init xs)        ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr



-- TODO remove, move functionality into Concatable
-- | A @(++)@-like append operation on 'Product's and 'Coproduct's. See also
-- ('.++.'). e.g.
--
-- > append ( (1,(2,())) , (3,(4,())) )  ==  (1,(2,(3,(4,()))))
--
-- > s :: Either (Bool, ()) (Either (Char, ()) (Either (Int, ()) ()))
-- > s = append ( Right $ Left (1,()) :: Either  (Either (Bool,()) (Char,()))  (Either (Int,()) ()) )
class (NormalConstr xs ~ NormalConstr ys) => Appendable xs ys where
    type xs :++: ys   
    append :: (t ~ NormalConstr xs)=> t xs ys -> (xs :++: ys)

instance (Product us)=> Appendable () us where
    type () :++: us = us 
    append = snd

instance (Product us, Appendable ts us)=> Appendable (a,ts) us where
    type (a,ts) :++: us = (a, ts :++: us) 
    append = fmap append . associate

instance (Coproduct us)=> Appendable (Either a ()) us where
    type Either a () :++: us = Either a (Either () us)
    append = associate
instance (Coproduct us)=> Appendable (Either a (b,c)) us where
    type Either a (b,c) :++: us = Either a (Either (b,c) us)
    append = associate

instance (Appendable (Either b ts) us)=> Appendable (Either a (Either b ts)) us where
    type Either a (Either b ts) :++: us = Either a (Either b ts :++: us)
    append = fmap append . associate

{-
infixr 5 .++.
-- | A convenience operator for concating two product types.
-- 
-- > (.++.) = curry append
(.++.) :: (Product xs, Product ys, Appendable xs ys)=> xs -> ys -> xs :++: ys
(.++.) = curry append
-}


single :: a -> (a,())
single a = (a,())


infixr 5 :>*<:, :*<:, >*<, *<

-- | Algebraic multiplication of two 'Normal' form types @xs@ and @ys@. When
-- both are 'Product's this operation is like the Prelude @(++)@. When both are
-- 'Coproduct's the ordering of constructors follow the \"FOIL\" pattern, e.g.
-- @(a + b + c)*(x + y) == (ax + ay + bx + by + cx + cy)@
--
-- Just like normal multiplication, this is a monoid with @()@ as our identity
-- object.
class Multiply xs ys where
    type xs :>*<: ys
    -- | Multiply 'Normal' types.
    (>*<) :: xs -> ys -> xs :>*<: ys

instance Multiply () ys where
    type () :>*<: ys = ys
    _ >*< ys = ys

instance (Product xs, DistributeTerm (xs :>*<: ys), Multiply xs ys)=> Multiply (x,xs) ys where 
    type (x,xs) :>*<: ys = x :*<: xs :>*<: ys
    (x,xs) >*< ys = x *< xs >*< ys

instance (Multiply as ys, Multiply bss ys, Concatable (Either (as :>*<: ys) (bss :>*<: ys))
    )=> Multiply (Either as bss) ys where
    -- type (Either as bss) :>*<: ys = (as :>*<: ys) :>*<: (bss :>*<: ys)
    type (Either as bss) :>*<: ys = Concated (Either (as :>*<: ys) (bss :>*<: ys))
    e >*< ys = concat $ bimap (>*< ys) (>*< ys) e


type family a :*<: xs
type instance a :*<: () = (a,())
type instance a :*<: (x,xs) = (a,(x,xs))
type instance a :*<: Either xs yss = Either (a :*<: xs) (a :*<: yss)

type family xs :>*: a
type instance () :>*: a = (a,())
type instance (x,xs) :>*: a = (x, xs :>*: a)
type instance Either xs yss :>*: a = Either (xs :>*: a) (yss :>*: a)

-- | Algebraic multiplication of a term with some 'Normal' type @xs@. When @xs@
-- is a 'Product' these are simple Cons/Snoc. For 'Coproeuct's the operation is
-- distributed over all constructors, e.g. @a*(x + y + z) = (ax + ay + az)@
class DistributeTerm xs where
    -- | prepend the term @a@.
    (*<) :: a -> xs -> a :*<: xs
    -- | append the term @a@.
    (>*) :: xs -> a -> xs :>*: a

instance DistributeTerm () where
    (*<) a as = (a, as)
    (>*) () a = (a,())
    
instance DistributeTerm xs=> DistributeTerm (x,xs) where
    (*<) a as = (a, as)
    (>*) (x,xs) a = (x, xs >* a)

instance (DistributeTerm xs, DistributeTerm yss)=> DistributeTerm (Either xs yss) where
    (*<) a = bimap (a *<) (a *<)
    (>*) as a = bimap (>* a) (>* a) as




infixl 5 |>
infixr 5 <| 
infixr 5 <!
-- | A convenience operator for appending an element to a product type.
-- 'Shiftable' generalizes this operation. See also 'Multiply'.JJ
--
-- > xs |> x = shiftl (x,xs)
(|>) :: (Product xs, Shiftable (x,xs))=> xs -> x -> (xs :> x)
xs |> x = shiftl (x,xs)

-- | A left push for Products.
--
-- > (<|) = (,)
(<|) ::  (Product xs)=> x -> xs -> (x,xs)
(<|) = (,)

-- | Convenience function for combining 'Product' terms, with ('<|'), e.g.
-- @0 <| 1 <| 2 <! 3@
--
-- > x <! y = (x,(y,()))
(<!) :: x -> y -> (x,(y,()))
x <! y = (x,(y,()))

-- | > constructorsOfNormal = 'unfanin' id
--
-- See also 'constructorsOf'. E.g.
--
-- > constructorsOfNormal ('a',('b',())) 'x' 'y'  ==  ('x',('y',()))
constructorsOfNormal :: (Exponent r)=> r -> (r :=>-> r)
constructorsOfNormal r = unfanin (id . (`asTypeOf` r))


-- NOTE: must not export constructor, 
--       must not derive instances that can modify length
newtype FixedList length a = FixedList { fixedList :: [a] } 
    deriving (Functor, Foldable, Traversable)

-- TODO Show instance, after e.g. Data.Set

-- NOTE: this hasn't came up as obviously useful until we wanted to store the
--       length of the empty product in FixedList. For now we'll not export this.
data Zero

type family Length t
type instance Length () = Zero                              -- | 0
type instance Length (a,()) = ()                            -- | 1
type instance Length (a,(b,bs)) = Either () (Length (b,bs)) -- | 1 + (length tail)

-- TODO replicate should take a Proxy (Constant a)?
-- replicate :: (Constant c)=> Proxy c -> a -> Replicated c a
type family Replicated len a
type instance Replicated Zero a = ()
type instance Replicated () a = (a,())
type instance Replicated (Either () n) a = (a, Replicated n a)



-- | A class for homogeneous 'Product's with terms all of type @a@.
class (Product as)=> Homogeneous a as | as -> a where
    -- | "Fill" a product with an initial value. If the size of the resulting
    -- product can't be inferred from context, provide a sype signature:
    --
    -- > truths = replicate True :: (Bool,(Bool,(Bool,())))
    --
    -- An n-ary @codiag@. See also 'extract' for 'Coproduct's
    replicate :: a -> as

    -- | Convert a homogeneous product to a fixed-length list.
    toFixedList :: as -> FixedList (Length as) a
    -- | Convert a list back into a homogeneous 'Product'.
 -- fromFixedList :: FixedList (Length as) a -> as -- but for type inferrence...
    fromFixedList :: (as ~ Replicated len a, len ~ Length as 
                     )=> FixedList len a -> Replicated len a

instance Homogeneous a () where
    replicate _ = ()
    toFixedList () = FixedList []
    fromFixedList (FixedList []) = ()

instance (Homogeneous a as, Replicated (Length as) a ~ as)=> Homogeneous a (a,as) where
    replicate a = (a,replicate a)
    toFixedList (a,as) = FixedList (a : (fixedList $ toFixedList as))
    fromFixedList (FixedList (a:as)) = (a,fromFixedList $ FixedList as)


-- | Factor out and return the 'Product' from a homogeneous 'Coproduct'. An
-- n-ary @codiag@.
--
-- See also 'replicate' for 'Product's.
--
-- > extract = fst . factorPrefix
extract :: (FactorPrefix t (Either t ts), Constant ((Either t ts) :/ t))=> Either t ts -> t
extract = fst . factorPrefix


-- TODO we'd get better inferrence if we can make FactorPrefix look like:
--   class (Product ab)=> FactorPrefix ab abcs cs | ab abcs -> cs, ab cs -> abcs, abcs cs -> ab where
-- and then could we combine 'multiply with this class?

-- | A 'Product' or 'Coproduct' @abcs@ out of which we can factor the product
-- @ab@, leaving some quotient.
class (Product ab)=> FactorPrefix ab abcs where
    -- | The quotient of @ab@ factored from @abcs@
    type abcs :/ ab
    factorPrefix :: abcs -> (ab, abcs :/ ab)

instance FactorPrefix () (x,y) where
    type (x,y) :/ () = (x,y)
    factorPrefix = (,) ()
instance FactorPrefix () () where
    type () :/ () = ()
    factorPrefix = (,) ()

instance (FactorPrefix bs bcs)=> FactorPrefix (a,bs) (a,bcs) where
    type (a,bcs) :/ (a,bs) = bcs :/ bs
    factorPrefix (a,bcs) = first ((,) a) $ factorPrefix bcs

instance (FactorPrefix (x,y) abc, FactorPrefix (x,y) abcs
         )=> FactorPrefix (x,y) (Either abc abcs) where
    type Either abc abcs :/ (x,y) = Either (abc :/ (x,y)) (abcs :/ (x,y))
    factorPrefix (Left abc) = fmap Left $ factorPrefix abc 
    factorPrefix (Right abcs) =  fmap Right $ factorPrefix abcs
instance (FactorPrefix () abc, FactorPrefix () abcs
         )=> FactorPrefix () (Either abc abcs) where
    type Either abc abcs :/ () = Either (abc :/ ()) (abcs :/ ())
    factorPrefix (Left abc) = fmap Left $ factorPrefix abc 
    factorPrefix (Right abcs) =  fmap Right $ factorPrefix abcs

-- | 'Coproduct's of the unit type are our constants in the algebra of ADTs.
-- They are cardinal numbers at the type level (length), while their /values/
-- are ordinal numbers (indicating position).
class Constant c
instance Constant ()
instance (Constant c)=> Constant (Either () c)
