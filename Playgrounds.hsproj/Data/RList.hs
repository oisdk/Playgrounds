{-# language LambdaCase      #-}
{-# language BangPatterns    #-}
{-# language DeriveFunctor   #-}

module Data.RList where
  
import Data.Monoid
import Data.Ord
import Data.Foldable
import Control.Monad
import Control.Applicative

replicateA :: Applicative f => Int -> f a -> f (RList a) -> f (RList a)
replicateA n action tail = go n where
  go m | m <= 0 = tail
       | otherwise = Cons 1 <$> action <*> (go (m-1))


data RList a = Nil
             | Cons !Int a (RList a)
             deriving (Eq, Functor)
             
instance Foldable RList where
  foldMap k (Cons n a xs) = f (k a) n <> foldMap k xs where
   f x y 
    | even y = f (x <> x) (y `quot` 2)
    | y == 1 = x
    | otherwise = g (x <> x) (pred y  `quot` 2) x
   g x y z 
    | even y = g (x <> x) (y `quot` 2) z
    | y == 1 = x <> z
    | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)
  foldMap _ Nil = mempty
  length Nil = 0
  length (Cons n _ xs) = n + length xs
  elem x (Cons _ y xs) = x == y || elem x xs
  elem _ _ = False
  maximum (Cons _ x xs) = go x xs where
    go !y Nil = y
    go !y (Cons _ z zs) = go (max y z) zs
  maximum _ = error "maximum of empty"
  minimum (Cons _ x xs) = go x xs where
    go !y Nil = y
    go !y (Cons _ z zs) = go (min y z) zs
  minimum _ = error "minimum of empty"
  sum = go 0 where
    go !n Nil = n
    go !n (Cons m x xs) = go (n + (fromIntegral m * x)) xs
  product = go 1 where
    go !n Nil = n
    go !n (Cons m x xs) = go (n * (x ^ m)) xs
    
instance Traversable RList where
  traverse f Nil = pure Nil
  traverse f (Cons n x xs) = replicateA n (f x) (traverse f xs)

instance Ord a => Ord (RList a) where
  compare = comparing (foldr (:) [])
    
instance Show a => Show (RList a) where
  show = show . foldr (:) []

uncons :: RList a -> Maybe (a, RList a)
uncons Nil = Nothing
uncons (Cons 1 x xs) = Just (x, xs)
uncons (Cons n x xs) = Just (x, Cons (n-1) x xs)

instance Eq a => Monoid (RList a) where
  mempty = Nil
  mappend (Cons n x Nil) (Cons m y ys) | x == y = Cons (n+m) y ys
  mappend (Cons n x xs) ys = Cons n x (xs `mappend` ys)
  mappend Nil xs = xs

fromList :: Eq a => [a] -> RList a
fromList [] = Nil
fromList (x:xx:xs) | x /= xx = Cons 1 x (fromList (xx:xs))
fromList (x:xs) = f x (fromList xs) where
  f e (Cons n x xs) | e == x = Cons (n+1) x xs
  f e xs = Cons 1 e xs
  
compress :: Eq a => RList a -> RList a
compress Nil = Nil
compress (Cons n x (Cons m y xs)) | x == y = compress (Cons (n+m) x xs)
compress (Cons n x xs) = Cons n x (compress xs)