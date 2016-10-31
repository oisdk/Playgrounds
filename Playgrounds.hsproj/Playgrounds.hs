{-# LANGUAGE DeriveFunctor #-}
{-# Language LambdaCase #-}

import Data.Map.Strict (Map, alter, fromList, singleton)
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix, uncons)
import Data.Semiring
import Data.Maybe
import Data.Foldable
import Control.Monad
import Prelude hiding (lookup)

data SplitResult a = SplitResult
  { common :: [a]
  , lTail  :: [a]
  , rTail  :: [a] 
  } deriving Show

prepCommon :: a -> SplitResult a -> SplitResult a
prepCommon x ~(SplitResult xs ys zs) = SplitResult (x:xs) ys zs

followSplit :: Eq a => [a] -> [a] -> SplitResult a
followSplit (x:xs) (y:ys)
  | x == y = prepCommon x (followSplit xs ys)
followSplit xs ys = SplitResult [] xs ys

data Trie a b = Trie
  { endsHere :: b
  , prefixSt :: [a]
  , children :: Map a (Trie a b)
  } deriving (Show, Eq)
      
data TrieEnd a b = TrieEnd { string :: [a], child :: Trie a b } deriving (Show, Eq)

lookup :: (Ord a, Semiring b) => [a] -> Trie a b -> b
lookup xs (Trie e p m) = case stripPrefix p xs of
  Nothing -> zero
  Just [] -> e
  Just (y:ys) -> getSum $ foldMap (Sum . lookup ys) (Map.lookup y m)
  
lookup [] = endsHere
lookup (x:xs) = maybe zero f . Map.lookup x . children where
  f (TrieEnd ys t) = maybe zero (flip lookup t) (stripPrefix (toList ys) xs)

emptyT :: Semiring b => Trie a b
emptyT = Trie zero Map.empty

spSingleton :: Semiring b => [a] -> TrieEnd a b
spSingleton xs = TrieEnd xs (Trie one Map.empty)

insert :: (Ord a, Semiring b) => [a] -> Trie a b -> Trie a b
insert []     (Trie _ c) = Trie one c
insert (x:xs) (Trie e c) = Trie e (alter (Just . maybe (spSingleton xs) f) x c) where
  f (TrieEnd ys t) = g (followSplit xs ys) where
    g (SplitResult cs xs ys) = (TrieEnd cs . insert xs . maybe t (uncurry h) . uncons) ys
    h y ys = Trie zero (singleton y (TrieEnd ys t))

fromListT :: (Ord a, Semiring b) => [[a]] -> Trie a b
fromListT = foldr insert emptyT

insertIsMember :: Trie Int Bool -> [Int] -> Bool
insertIsMember t xs = lookup xs (insert xs t)

orderDoesntMatter :: [[Int]] -> Bool
orderDoesntMatter xs = fromListT xs == fromListT (reverse xs)

instance (Arbitrary a, Ord a, Arbitrary b, Semiring b) => Arbitrary (Trie a b) where
  arbitrary = fromListT <$> arbitrary
  


