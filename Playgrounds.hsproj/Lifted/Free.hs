{-# language RankNTypes        #-}
{-# language ConstraintKinds   #-}
{-# language FlexibleInstances #-}
            
module Lifted.Free where
      
import Data.Semigroup
              
newtype Free c a
  = Free
  { runFree :: forall b. c b => (a -> b) -> b }
              
instance Foldable (Free Monoid) where
  foldMap = flip runFree
      
instance Foldable (Free Semigroup) where
  foldMap f = unwrapMonoid . flip runFree (WrapMonoid . f)
    
instance Functor (Free c) where
  fmap f (Free x) = Free (\g -> x (g . f))
          
instance Applicative (Free c) where
  pure x = Free (\k -> k x)
  Free fs <*> Free xs = Free (\k -> fs (\c -> xs (k . c)))
        
instance Monad (Free f) where
  m >>= k = Free (\c -> runFree m (\a -> runFree (k a) c))
