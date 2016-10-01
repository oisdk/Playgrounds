module Control.Applicative.Alternative where

import Control.Applicative
import Data.Foldable

ensure :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
ensure p xs = xs >>= (\x -> if p x then pure x else empty)

toAlt :: (Foldable f, Alternative m) => f a -> m a
toAlt = foldr ((<|>) . pure) empty

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = fmap Left x <|> fmap Right y

mapMaybe :: (Foldable f, Monad m, Alternative m) => (a -> f b) -> m a -> m b
mapMaybe f xs = xs >>= (toAlt . f)