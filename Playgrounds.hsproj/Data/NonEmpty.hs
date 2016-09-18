{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language OverloadedLists #-}
{-# language TypeFamilies #-}

module Data.NonEmpty where
  
import GHC.Exts

infixr 5 :>
data NonEmpty a = Single a
                | a :> (NonEmpty a)
                deriving (Functor, Foldable, Traversable, Eq, Ord)
  
infixr 5 `append`
append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append (Single x) ys = x :> ys
append (x :> xs) ys = x :> (xs `append` ys)
              
instance Applicative NonEmpty where
  pure = Single
  Single f <*> xs = fmap f xs
  f :> fs <*> xs = fmap f xs `append` (fs <*> xs)
  
instance Monad NonEmpty where
  Single x >>= f = f x
  x :> xs >>= f = f x `append` (xs >>= f)
  
instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  toList = foldr (:) []
  fromList [] = error "NonEmpty made from empty list"
  fromList [x] = Single x
  fromList (x:xs) = x :> fromList xs
  
instance Show a => Show (NonEmpty a) where
  show = show . toList
  
head :: NonEmpty a -> a
head (Single x) = x
head (x :> _) = x