{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Shapely.Category
    where

-- ========================================================================
-- private module, copy-pasted from Edward A. Kmett's "categories" package,
-- version 1.0.5, licensed BSD3.
-- ========================================================================


-- from Control.Category.Associative ---------------------------------------
--class Bifunctor p k k k => Associative k p where
class Associative k p where
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
