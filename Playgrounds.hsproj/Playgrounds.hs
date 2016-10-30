{-# LANGUAGE DeriveFunctor #-}

import Data.Map.Strict (Map, lookup, alter, fromList, singleton)
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Prelude hiding (lookup)
import Data.Maybe
import Test.QuickCheck

data SplitResult a = SplitResult
  { common :: [a]
  , lTail  :: [a]
  , rTail  :: [a] 
  } deriving Show

both :: ([a] -> [a]) -> SplitResult a -> SplitResult a
both f s = SplitResult (f (common s)) (lTail s) (rTail s)

followSplit :: Eq a => [a] -> [a] -> SplitResult a
followSplit (x:xs) (y:ys) | x == y = both (x:) (followSplit xs ys)
followSplit xs ys = SplitResult [] xs ys

data Trie a = Trie
  { endsHere :: Bool
  , children :: Map a ([a],Trie a)
  } deriving (Show, Eq)
  
member :: Ord a => [a] -> Trie a -> Bool
member [] t = endsHere t
member (x:xs) (Trie _ c) = maybe False f (lookup x c) where
  f (ys,t) = maybe False (flip member t) (stripPrefix ys xs)


emptyT :: Trie a
emptyT = Trie False Map.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ c) = Trie True c
insert (x:xs) (Trie e c) = Trie e (alter f x c) where
  f Nothing = Just (xs,Trie True Map.empty)
  f (Just (ys,t)) = case followSplit xs ys of
    SplitResult cs xs [] -> Just (cs, insert xs t)
    SplitResult cs [] (y:ys) -> Just (cs, Trie True (singleton y (ys,t)))
    SplitResult cs (x:xs) (y:ys) -> Just (cs, Trie False (fromList   [(x,(xs,Trie True Map.empty)), (y,(ys,t))]))
    
instance (Arbitrary a, Ord a) => Arbitrary (Trie a) where
  arbitrary = (foldr insert emptyT :: Ord a => [[a]] -> Trie a) <$> arbitrary 
  
insertIsIn :: Ord a => Trie a -> [a] -> Bool
insertIsIn t xs = member xs (insert xs t)

sameOrder :: Ord a => [[a]] -> Bool
sameOrder xs = foldr insert emptyT xs == foldr insert emptyT (reverse xs)