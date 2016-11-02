{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Complexity where

import Data.Bool
import Control.Arrow (first, second)
import GHC.TypeLits
import Prelude hiding (Functor(..), Applicative(..), Monad(..), length, (<$>))
import Prelude (fail)
import Control.Monad.Restricted

newtype Cost (n :: Nat) a = Cost { unCost :: a }

instance Functor (Cost n) a b where
  fmap f (Cost x) = Cost (f x)

instance Pointed (Cost 0) a where
  pure = Cost

instance Apply 

single :: x -> Cost 1 x
single = Cost

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

length :: Vec n a -> Cost n Int
length Nil = return 0
length (Cons _ xs) = do
  tailLen <- length xs
  single (1 + tailLen)
  
singleLt :: Ord a => a -> a -> Cost 1 Bool
singleLt x y = single (x < y)

(<$>) :: (a -> b) -> Cost n a -> Cost n b
f <$> (Cost x) = Cost (f x)

(<*>) :: Cost n (a -> b) -> Cost m a -> Cost (m + n) b
Cost f <*> Cost x = Cost (f x)

partitionA :: (a -> Cost 1 Bool) -> Vec n a -> Cost n ([a],[a])
partitionA f Nil = return ([], [])
partitionA f (Cons x xs) =
  bool (first (x:)) (second (x:)) <$> f x <*> partitionA f xs
