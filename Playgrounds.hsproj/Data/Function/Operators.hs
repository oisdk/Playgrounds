module Data.Function.Operators
  ( ($)
  , (&)
  , (<<<)
  , (>>>)
  , (<=<)
  , (>=>)
  , (<-<)
  , (>->)
  , (.:)
  ) where
  
import Control.Arrow
import Control.Monad

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

infixl 1 <-<
(<-<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <-< f = fmap g <<< f

infixr 1 >->
(>->) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f >-> g = f >>> fmap g

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)