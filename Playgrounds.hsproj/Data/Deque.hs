{-# language DeriveFunctor #-}
{-# language PatternSynonyms, ViewPatterns #-}


module Data.Deque where

import Data.Monoid
import Control.Monad
import Data.Foldable
import Control.Applicative.Backwards

data Deque a = Deque
  { front :: [a]
  , back  :: [a] 
  } deriving Functor
  
instance Foldable Deque where
  foldr f b (Deque xs ys) = foldr f (foldl' (flip f) b ys) xs
  foldMap f (Deque xs ys) = foldMap f xs <> foldl' (\a e -> f e <> a) mempty ys

instance Traversable Deque where
  traverse f (Deque xs ys) =
    Deque <$> traverse f xs
          <*> forwards (traverse (Backwards . f) ys)

reverse' :: Deque a -> Deque a
reverse' (Deque xs ys) = Deque ys xs

instance Monoid (Deque a) where
  mempty = Nil
  xs `mappend` ys = foldr (:<)  ys xs

instance Applicative Deque where
  pure x = Deque [x] []
  (<*>) = ap
  
instance Monad Deque where
  xs >>= f = foldr (mappend . f) Nil xs
  
infixl 5 :>
pattern (:>) :: Deque a -> a -> Deque a
pattern xs :> x <- (popLast -> Just (xs,x)) where
  Deque xs ys :> x = Deque xs (x:ys)
  
infixr 5 :<
pattern (:<) :: a -> Deque a -> Deque a
pattern x :< xs <- (popHead -> Just (x,xs)) where
  x :< Deque xs ys = Deque (x:xs) ys
  
pattern Nil :: Deque a
pattern Nil = Deque [][]

popLast :: Deque a -> Maybe (Deque a, a)
popLast (Deque xs (y:ys)) = Just (Deque xs ys, y)
popLast (Deque [] []) = Nothing
popLast (Deque xs []) = popLast (Deque [] (reverse xs))

popHead :: Deque a -> Maybe (a, Deque a)
popHead (Deque (x:xs) ys) = Just (x, Deque xs ys)
popHead (Deque [] []) = Nothing
popHead (Deque [] ys) = popHead (Deque (reverse ys) [])

instance Show a => Show (Deque a) where
  show = show . toList
  
fromList :: Foldable f => f a -> Deque a
fromList = flip Deque [] . toList