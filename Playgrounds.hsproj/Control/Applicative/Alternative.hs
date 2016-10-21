module Control.Applicative.Alternative where

import Control.Applicative
import Data.Function.Operators
import Data.Foldable
import Data.Bool
import Control.Monad

-- | forall (Foldable f, Alternative m) =>
-- f    a  -> m a  = choice
-- f (m a) -> m a  = asum

-- | For lists, behaves like @filter@ (although a little slower). 
-- @mfilter@ for alternatives.
filterA :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
filterA p = (=<<) ((<$) <*> guard . p)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p x = x <$ guard (p x)

choice :: (Foldable f, Alternative m) => f a -> m a
choice = foldr ((<|>) . pure) empty

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = fmap Left x <|> fmap Right y

mapMaybe :: (Foldable f, Monad m, Alternative m) => (a -> f b) -> m a -> m b
mapMaybe = (=<<) . (.) choice