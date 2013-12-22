{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Main
    where
 
import Data.Shapely
import Data.Bits

-- This example copied from: 
--   http://www.haskell.org/haskellwiki/GHC.Generics#Complete_working_example
-- See commit history for changes required to port to shapely-data.
 
 
data Bit = O | I deriving Show
 
class Serialize a where
  put :: a -> [Bit]
 
  default put :: (Shapely a, GSerialize (Normal a)) => a -> [Bit]
  put a = gput (from a)
 
  get :: [Bit] -> (a, [Bit])
 
  default get :: (Shapely a, GSerialize (Normal a)) => [Bit] -> (a, [Bit])
  get xs = (to x, xs')
    where (x, xs') = gget xs
 
class GSerialize f where
  gput :: f -> [Bit]
  gget :: [Bit] -> (f, [Bit])
 
-- | Unit: used for constructors without arguments
instance GSerialize () where
  gput () = []
  gget xs = ((), xs)
 
-- | Constants, additional parameters and recursion of kind *
instance (Serialize a, GSerialize b) => GSerialize (a , b) where
  gput (a , b) = put a ++ gput b
  gget xs = ((a , b), xs'')
    where (a, xs') = get xs
          (b, xs'') = gget xs'
 
-- | Meta-information (constructor names, etc.)
instance (GSerialize a, GSerialize b) => GSerialize (Either a b) where
  gput (Left x) = O : gput x
  gput (Right x) = I : gput x
  gget (O:xs) = (Left x, xs')
    where (x, xs') = gget xs
  gget (I:xs) = (Right x, xs')
    where (x, xs') = gget xs
 
instance Serialize Bool where
  put True = [I]
  put False = [O]
  get (I:xs) = (True, xs)
  get (O:xs) = (False, xs)
 
--
-- Try it out. (Normally this would be in a separate module.)
--
 
data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
  deriving (Show)
$(deriveShapely ''UserTree)
 
instance (Serialize a) => Serialize (UserTree a)
 
main = do
  let xs = put True
  print (fst . get $ xs :: Bool)
  let ys = put (Leaf :: UserTree Bool)
  print (fst . get $ ys :: UserTree Bool)
  let zs = put (Node False Leaf Leaf :: UserTree Bool)
  print (fst . get $ zs :: UserTree Bool)
