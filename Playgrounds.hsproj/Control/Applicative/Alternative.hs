module Control.Applicative.Alternative where

import Control.Applicative
import Data.Function.Operators
import Data.Foldable
import Data.Bool

ensure :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
ensure p xs = xs >>= (flip bool empty . pure <*> p)

toAlt :: (Foldable f, Alternative m) => f a -> m a
toAlt = foldr ((<|>) . pure) empty

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = fmap Left x <|> fmap Right y

mapMaybe :: (Foldable f, Monad m, Alternative m) => (a -> f b) -> m a -> m b
mapMaybe = (=<<) . (.) toAlt