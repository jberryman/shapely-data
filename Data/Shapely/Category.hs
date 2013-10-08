{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-} --for swapFront
module Data.Shapely.Category
    where

-- ========================================================================
-- private module, copy-pasted from Edward A. Kmett's "categories" package,
-- version 1.0.5, licensed BSD3.
-- ========================================================================
import Prelude hiding (id, (.))
import Control.Category

-- from Control.Categorical.Bifunctor ----------------------------------------
class (Category r, Category t) => PFunctor p r t | p r -> t, p t -> r where
    first :: r a b -> t (p a c) (p b c)

class (Category s, Category t) => QFunctor q s t | q s -> t, q t -> s where
    second :: s a b -> t (q c a) (q c b)

class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
    bimap :: r a b -> s c d -> t (p a c) (p b d)

instance PFunctor (,) (->) (->) where 
    first f = bimap f id
instance QFunctor (,) (->) (->) where 
    second = bimap id
instance Bifunctor (,) (->) (->) (->) where
    bimap f g (a,b)= (f a, g b)

instance PFunctor Either (->) (->) where 
    first f = bimap f id
instance QFunctor Either (->) (->) where 
    second = bimap id
instance Bifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)

instance QFunctor (->) (->) (->) where
    second = (.)


-- from Control.Category.Associative ---------------------------------------
class Bifunctor p k k k => Associative k p where
    associate :: k (p (p a b) c) (p a (p b c))
    disassociate :: k (p a (p b c)) (p (p a b) c)

instance Associative (->) (,) where
    associate ((a,b),c) = (a,(b,c))
    disassociate (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
    associate (Left (Left a)) = Left a
    associate (Left (Right b)) = Right (Left b)
    associate (Right c) = Right (Right c)
    disassociate (Left a) = Left (Left a)
    disassociate (Right (Left b)) = Left (Right b)
    disassociate (Right (Right c)) = Right c

swapFront :: Symmetric (->) p => p b (p a c) -> p a (p b c)
swapFront = associate . first swap . disassociate


-- from Control.Category.Braided -------------------------------------------

class Associative k p => Braided k p where
    braid :: k (p a b) (p b a)

instance Braided (->) Either where
    braid (Left a) = Right a
    braid (Right b) = Left b

instance Braided (->) (,) where
    braid ~(a,b) = (b,a)

class Braided k p => Symmetric k p

swap :: Symmetric k p => k (p a b) (p b a)
swap = braid

instance Symmetric (->) Either
instance Symmetric (->) (,)
