{-# LANGUAGE DeriveFunctor #-}
{-# Language LambdaCase #-}

import Data.Map.Strict (Map, lookup, alter, fromList, singleton)
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Prelude hiding (lookup)
import Data.Maybe
import Data.Foldable

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

data Trie a = Trie
  { endsHere :: Bool
  , children :: Map a ([a],Trie a)
  } deriving (Show, Eq)
  
member :: Ord a => [a] -> Trie a -> Bool
member [] t = endsHere t
member (x:xs) (Trie _ c) = maybe False f (lookup x c) where
  f (ys,t) = maybe False (flip member t) (stripPrefix (toList ys) xs)

emptyT :: Trie a
emptyT = Trie False Map.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ c) = Trie True c
insert (x:xs) (Trie e c) = Trie e (alter f x c) where
  f Nothing = Just (xs,Trie True Map.empty)
  f (Just (ys,t)) = let (SplitResult cs xxs yys) = followSplit xs ys in Just (cs, g xxs yys) where
    g xs [] = insert xs t
    g [] (y:ys) =  Trie True (singleton y (ys,t))
    g (x:xs) (y:ys) = Trie False (fromList [(x,(xs,Trie True Map.empty)), (y,(ys,t))])

fromListT :: Ord a => [[a]] -> Trie a
fromListT = foldr insert emptyT

insertIsMember :: Trie Int -> [Int] -> Bool
insertIsMember t xs = member xs (insert xs t)

orderDoesntMatter :: [[Int]] -> Bool
orderDoesntMatter xs = fromListT xs == fromListT (reverse xs)

instance (Arbitrary a, Ord a) => Arbitrary (Trie a) where
  arbitrary = fromListT <$> arbitrary