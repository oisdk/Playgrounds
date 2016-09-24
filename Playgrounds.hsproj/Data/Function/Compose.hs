module Data.Function.Compose
  ( (>>>)
  , (<<<)
  , (<|)
  , (|>)
  , (<=<)
  , (>=>)
  , (<$<)
  , (>$>)
  ) where
  
import Control.Arrow
import Control.Monad

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 1 <$<
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <$< f = fmap g <<< f

infixl 1 >$>
(>$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f >$> g = f >>> fmap g