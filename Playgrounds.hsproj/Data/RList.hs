{-# language LambdaCase      #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns    #-}

module Data.RList where
  
import Data.Monoid
import Data.Ord

data RList a = Nil
             | Cons a Int (RList a)
             deriving (Eq)

instance Foldable RList where
  foldr f b = r where
    r Nil = b
    r (Cons x n xs) = rep n x (r xs)
    rep 1 x y = f x y
    rep n x y = f x (rep (n-1) x y)
  length Nil = 0
  length (Cons _ n xs) = n + length xs
  
instance Ord a => Ord (RList a) where
  compare = comparing (foldr (:) [])
    
instance Show a => Show (RList a) where
  show = show . foldr (:) []

uncons :: RList a -> Maybe (a, RList a)
uncons Nil = Nothing
uncons (Cons x 1 xs) = Just (x, xs)
uncons (Cons x n xs) = Just (x, Cons x (n-1) xs)

infixr 5 :>
pattern (:>) :: () => Eq a => a -> RList a -> RList a
pattern x :> xs <- (uncons -> Just (x, xs)) where
  x :> (Cons y n xs) | x == y = Cons y (n+1) xs
  x :> xs = Cons x 1 xs

instance Eq a => Monoid (RList a) where
  mempty = Nil
  mappend (Cons x n Nil) (Cons y m ys) | x == y = Cons y (n+m) ys
  mappend (Cons x n xs) ys = Cons x n (xs `mappend` ys)
  mappend Nil xs = xs

fromList :: Eq a => [a] -> RList a
fromList [] = Nil
fromList (x:xx:xs) | x /= xx = Cons x 1 (fromList (xx:xs))
fromList (x:xs) = f x (fromList xs) where
  f e (Cons x n xs) | e == x = Cons x (n+1) xs
  f e xs = Cons e 1 xs