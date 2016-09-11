module Control.Monad.Split where
  
import Prelude hiding (Functor(..), Applicative(..), Monad(..), (<$>))

class Functor f where
  fmap  :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b

  fmap  = (<$>)
  (<$>) = fmap
  
class Pointed f where
  pure :: a -> f a
  
class Functor f => Apply f where
  (<*>) :: f (a -> b) -> f a -> f b
  (<^>) :: f a -> f b -> f (a,b)
  
  f <*> x = fmap (uncurry ($)) (f <^> x)
  x <^> y = (,) <$> x <*> y
  
class (Pointed f, Apply f) => Applicative f

class Applicative f => Monad f where
  join  :: f (f a) -> f a
  (<=<) :: (b -> f c) -> (a -> f b) -> a -> f c
  (>=>) :: (a -> f b) -> (b -> f c) -> a -> f c
  (=<<) :: (a -> f b) -> f a -> f b
  (>>=) :: f a -> (a -> f b) -> f b
  
  join x = x >>= id
  (f >=> g) x = f x >>= g
  (g <=< f) x = f x >>= g
  x >>= f = join (fmap f x)
  f =<< x = join (fmap f x)
  
instance Functor [] where
  fmap = map
  
instance Pointed [] where
  pure x = [x]
  
instance Apply [] where
  fs <*> xs = [ f x | f <- fs, x <- xs ]
  
instance Applicative []

instance Monad [] where
  join = concat
  