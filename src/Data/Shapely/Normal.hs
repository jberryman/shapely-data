{-# LANGUAGE TypeOperators #-}  
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}  -- for nested Type family: Reversed (Tail t) :> Head t
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

You probably want to import this in one of the following ways:

> import Data.Shapely.Normal as Sh
> import qualified Data.Shapely.Normal as Sh
> import Data.Shapely.Normal hiding ((!!),repeat,replicate,concat,reverse, map, length)

/NOTE/: The structure of the classes, type functions, and class constraints
here are likely to change a lot.
-}
      Only(..)

    -- * Reordering 'Product' and 'Sum' terms
    , Reversable(..)
    , Shiftable(..)
    , viewr
 -- , Appendable(..), Concatable(..)
    -- ** Convenience Type synonyms
    , (:*:), (:*!), (:+:)

    -- * Operations on Products
    -- ** Homogeneous (list-like) products
    , Homogeneous(..), FixedList(), (!!), ($$:), replicate
    -- ** Construction convenience operators
    , single
    , (*:), (*!)
    -- ** Forcing types
    , HavingLength, ary

    -- * Product and Sum Conversions
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
    -- exponentiation, where @xᵇ == (b -> x)@. The operations here come from
    -- translating the algebraic laws of exponents to their equivalents on
    -- ADTs.
    , Exponent(..), (:=>->), Base(..), (:->=>)
    , constructorsOfNormal

    -- ** Constants
    , Constant, Length, Replicated
    -- *** Cardinals
    , One, Two, Three, Four, Five, Six, Seven
    , _1, _2, _3, _4, _5, _6, _7
    -- *** Ordinals
    , OneOrMore(..), _2nd, _3rd, _4th, _5th, _6th, _7th
    -- *** Forcing types
    , length, _of
  --, -+- , succ, (TODO math with cardinals/ordinals)
    ) where

import Data.Shapely.Category
import Data.Shapely.Normal.Classes
import Data.Shapely.Normal.Massageable
import Data.Shapely.Normal.Exponentiation
import Control.Applicative() -- Functor instances for (,) and Either

import Prelude hiding ((!!),repeat,replicate,concat,reverse, map, length)
import qualified Prelude 

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import Data.Proxy

-- TODO:
--
--   v0.2:
--      - move to closed type families, look at replacing OverlappingInstances
--        with these by using families for type equality
--          - use closed type fams in proxy-kindness too
--          - does this give us some injectivity / better inferrence yet?
--              (WON'T. apparently work not being done on this yet)
--      - in class declarations, consider removing constraints (but keeping
--         them on instances) this will help clean up signatures on polymorphic
--         functions. This will probably be more efficient more of the time too.
--      - work on PatternFunctor stuff (see branch) now that we have closed TFs
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
--      - constraint kinds: hmmm...
--          can we somehow use this for predicate classes (we'd like that for Functor)
--            (allows constraints to be indexed by types...)
--          we should be able to do this with closed type families where a
--          nested (f source) (f target) evaluates to a Functor constraint, else ()
--          ...at least we can simplify constraints
--      - study "lens", and see how we can integrate/defer functionality to that lib
--          - also look at "vinyl"
--      - maybe an nthMap function on Sums
--      - Do we have nice inference on closed type families yet? revisit constraints
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




infixr 5 :+:
infixr 6 :*:
infixr 6 :*!
type (:+:) = Either
type (:*:) = (,)
type (x :*! y) = (x,(y,()))


-- TODO: use instance defaulting here for these definitions after they are settled?


-- | Note: @viewl@ would be simply @id@.
--
-- > viewr = swap . shiftr
viewr :: (Symmetric (->) p, Shiftable t, ShiftedR t ~ p a b) => t -> p b a
viewr = swap . shiftr

-- TODO: or name this class "higher-order normal" or something?
--       we should be able to do 'append' here with a match on ((x,xs),xss) .. ((),xss)... etc.
--       maybe remove this; confusing since Either instance not a Sum.
-- | Class for flattening a 'Product' of 'Product's, or a nested sum of
-- 'Sum's.
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

-- These are the two possible leaf Sums at the Last position of
-- our Either spine:
instance Concatable (Either () es) where
    type Concated (Either () es) = (Either () es)
    concat = id

instance Concatable (Either (x,ys) es) where
    type Concated (Either (x,ys) es) = (Either (x,ys) es)
    concat = id

-- | Note here that @xs@ is not a 'Sum', since it is a sum of sums.
instance (Concatable ess, Appendable (Either x ys) (Concated ess)
         , Sum (Either x ys), Sum (Concated ess)
    )=> Concatable (Either (Either x ys) ess) where
    type Concated (Either (Either x ys) ess) = Either x ys :++: Concated ess
    concat = append . fmap concat


-- | Reversing 'Products' and 'Sum's
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
        , Tail (Either x zs) ~ Tail xs
        , (Last xs :< (x :< Init xs)) ~ Either a0 (Either x c0)
        , (Last xs :< Init xs)        ~ Either a0 c0
        )=> Shiftable (Either x (Either y zs)) where
    shiftl = fmap shiftl . swapFront
    shiftr = swapFront . fmap shiftr



-- TODO remove, move functionality into Concatable?
-- | A @(++)@-like append operation on 'Product's and 'Sum's. See also
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

instance (Sum us)=> Appendable (Either a ()) us where
    type Either a () :++: us = Either a (Either () us)
    append = associate
instance (Sum us)=> Appendable (Either a (b,c)) us where
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
-- 'Sum's the ordering of constructors follow the \"FOIL\" pattern, e.g.
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
-- is a 'Product' these are simple Cons/Snoc (see '*:'). For 'Sum's the
-- operation is distributed over all constructors, as in: 
-- @a(x + y + z) = (ax + ay + az)@
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




infixr 5 *: 
infixr 5 *!

-- | A left push for Products.
--
-- > (*:) = (,)
--
-- For a right push, see ('>*').
(*:) ::  (Product xs)=> x -> xs -> (x,xs)
(*:) = (,)

-- | Convenience function for combining 'Product' terms, with ('*:'), e.g.
-- @0 *: 1 *: 2 *! 3@
--
-- > x *! y = (x,(y,()))
(*!) :: x -> y -> (x,(y,()))
x *! y = (x,(y,()))

-- | > constructorsOfNormal = 'unfanin' id
--
-- See also 'constructorsOf'. E.g.
--
-- > constructorsOfNormal ('a',('b',())) 'x' 'y'  ==  ('x',('y',()))
constructorsOfNormal :: (Exponent r)=> r -> (r :=>-> r)
constructorsOfNormal r = unfanin (id . (`asTypeOf` r))



-- NOTE: must not export constructor, 
--       must not derive instances that can modify length
-- | An opaque wrapper type allowing application of useful class methods on
-- 'Homogeneous' 'Product's. Only operations that don't modify the length of
-- the product (which is stored in the @len@ parameter) are supported.
newtype FixedList length a = FixedList { fixedList :: [a] } 
    deriving (Functor, Foldable, Traversable, Eq, Ord)

-- TODO constraints in fromFList might be stupid, as evidenced here
instance (as ~ Replicated len a, Homogeneous a as, Show as, len ~ Length as)=> Show (FixedList len a) where
    show l = "toFList " ++ show (fromFList l)

-- NOTE: this hasn't came up as obviously useful until we wanted to store the
--       length of the empty product in FixedList. For now we'll not export this.
data Zero


length :: (Product as)=> as -> Proxy (Length as)
length _ = Proxy

-- | Used as in e.g. @_3rd `_of` _7@, which has inferred type @Seven@.
_of :: c -> Proxy c -> c
_of = const

-- I PLZ CAN HAS TYP INFERENCE ON CLOZED TYP FAMYLYS?
-- | this inverts 'Length'
class (Product p, Length p ~ c)=> HavingLength c p | c -> p
instance HavingLength Zero ()
instance HavingLength () (a,())
instance (HavingLength c p, Length (a, p) ~ Either () c)=> HavingLength (Either () c) (a, p)

-- | > ary _ = id
--
-- Force the arity of an arity-polymorphic function on 'Product's. e.g.
--
-- >>> :t _3 `ary` shiftl
-- _3 `ary` shiftl :: (a, (a1, (a2, ()))) -> ShiftedL (a, (a1, (a2, ())))
ary :: (HavingLength c p)=> Proxy c -> (p -> x) -> (p -> x)
ary _ = id


type family Length t
-- | 0
type instance Length () = Zero                              
-- | 1
type instance Length (a,()) = ()                            
-- | 1 + ('length' tail)
type instance Length (a,(b,bs)) = Either () (Length (b,bs)) 

type family Replicated len a
type instance Replicated Zero a = ()
type instance Replicated () a = (a,())
type instance Replicated (Either () n) a = (a, Replicated n a)


infixr 0 $$:
-- | > ($$:) f = fromFList . f . toFList
($$:)
  :: (Length as ~ len, Replicated len b ~ bs, -- for cleaner type
      Homogeneous b bs, Homogeneous a as, Length bs ~ len) =>
     (FixedList len a -> FixedList len b) -> as -> bs
($$:) f = fromFList . f . toFList
    
-- | Replicate @a@, producing a 'Product' of length @len@.
--
-- > replicate _ = 'repeat'
replicate :: (Homogeneous a as, as ~ Replicated len a, len ~ Length as )=> 
              Proxy len -> a -> Replicated len a
replicate _ = repeat


-- | A class for homogeneous 'Product's with terms all of type @a@.
class (Product as)=> Homogeneous a as | as -> a where
    -- | "Fill" a product with an initial value. If the size of the resulting
    -- product can't be inferred from context, provide a sype signature:
    --
    -- > truths = repeat True :: (Bool,(Bool,(Bool,())))
    --
    -- An n-ary @codiag@. See also 'extract' for 'Sum's
    repeat :: a -> as

    -- | Convert a homogeneous product to a fixed-length list.
    toFList :: as -> FixedList (Length as) a
    -- | Convert a list back into a homogeneous 'Product'.
    fromFList :: (as ~ Replicated len a, len ~ Length as )=> 
                  FixedList len a -> Replicated len a
 -- fromFList :: FixedList (Length as) a -> as -- but for better type inferrence, above.
    

instance Homogeneous a () where
    repeat _ = ()
    toFList () = FixedList []
    fromFList (FixedList []) = ()
    fromFList _ = error "FixedList longer than stored length somehow! Please report this bug"

instance (Homogeneous a as, Replicated (Length as) a ~ as)=> Homogeneous a (a,as) where
    repeat a = (a,repeat a)
    toFList (a,as) = FixedList (a : (fixedList $ toFList as))
    fromFList (FixedList (a:as)) = (a,fromFList $ FixedList as)
    fromFList _ = error "FixedList shorter than stored length somehow! Please report this bug"


-- | Factor out and return the 'Product' from a homogeneous 'Sum'. An
-- n-ary @codiag@.
--
-- See also 'repeat' for 'Product's.
--
-- > extract = fst . factorPrefix
extract :: (FactorPrefix t (Either t ts), Constant ((Either t ts) :/ t))=> Either t ts -> t
extract = fst . factorPrefix


-- TODO we'd get better inferrence if we can make FactorPrefix look like:
--   class (Product ab)=> FactorPrefix ab abcs cs | ab abcs -> cs, ab cs -> abcs, abcs cs -> ab where
-- and then could we combine 'multiply with this class?

-- | A 'Product' or 'Sum' @abcs@ out of which we can factor the product
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

-- | Return the term at the 1-based index @n@ of the 'Homogeneous' 'Product' @xs@.
--
-- > as !! i = 'fanin' as (i `'_of'` 'length' as)
(!!) :: (Product as, as ~ (i :=>-> a), i ~ Length as, Exponent i) => as -> i -> a
as !! i = fanin as (i `_of` length as)

-- | 'Sum's of the unit type are our constants in the algebra of ADTs.
-- They are cardinal numbers at the type level (length), while their /values/
-- are ordinal numbers (indicating position).
class Constant c
instance Constant ()
instance (Constant c)=> Constant (Either () c)

type One = ()
type Two = One :+: One
type Three = One :+: Two
type Four = One :+: Three
type Five = One :+: Four
type Six = One :+: Five
type Seven = One :+: Six

_1 :: Proxy One
_1 = Proxy
_2 :: Proxy Two
_2 = Proxy
_3 :: Proxy Three
_3 = Proxy
_4 :: Proxy Four
_4 = Proxy
_5 :: Proxy Five
_5 = Proxy
_6 :: Proxy Six
_6 = Proxy
_7 :: Proxy Seven
_7 = Proxy

class Constant c => OneOrMore c where
    _1st :: c
instance OneOrMore () where
    _1st = ()
instance (Constant c)=> OneOrMore (Either () c) where
    _1st = Left ()

_2nd :: (OneOrMore c)=> One :+: c
_2nd = Right $ _1st
_3rd :: (OneOrMore c)=> One :+: One :+: c
_3rd = Right $ _2nd
_4th :: (OneOrMore c)=> One :+: One :+: One :+: c
_4th = Right $ _3rd
_5th :: (OneOrMore c)=> One :+: One :+: One :+: One :+: c
_5th = Right $ _4th
_6th :: (OneOrMore c)=> One :+: One :+: One :+: One :+: One :+: c
_6th = Right $ _5th
_7th :: (OneOrMore c)=> One :+: One :+: One :+: One :+: One :+: One :+: c
_7th = Right $ _6th
