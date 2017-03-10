module Control.Applicative.Alternative where

import Control.Applicative
import Data.Bool
import Data.Monoid
import Control.Monad


-- | A generalized version of 'filter', which works on anything which is
-- both a 'Monad' and 'Alternative'.
--
-- prop> \(Blind p) xs -> filter p xs === afilter p xs
afilter :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
afilter p = (=<<) (\x -> bool empty (pure x) (p x))

-- | 'ensure' allows you to attach a condition to something
ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p x = x <$ guard (p x)

-- | 'eitherA' is especially useful for parsers.
eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = fmap Left x <|> fmap Right y

-- | Convert any 'Foldable' to an 'Alternative'
toAlt :: (Alternative f, Foldable t) => t a -> f a
toAlt = getAlt . foldMap pure

-- | Map a function over a monad, and concat the results. This is a
-- generalized form of the function 'Data.Maybe.mapMaybe'.
mapAlt :: (Monad m, Alternative m, Foldable f) => (a -> f b) -> m a -> m b
mapAlt f = (=<<) (toAlt . f)

atLeast :: Alternative f => Int -> f a -> f [a]
atLeast m f = go (max 0 m) where
  go 0 = many f
  go n = liftA2 (:) f (go (n-1))
  
atMost :: Alternative f => Int -> f a -> f [a]
atMost m f = go (max 0 m) where
  go 0 = pure []
  go n = liftA2 (:) f (go (n-1)) <|> pure []

replicateA        :: (Applicative m) => Int -> m a -> m [a]
{-# INLINEABLE replicateA #-}
{-# SPECIALISE replicateA :: Int -> IO a -> IO [a] #-}
{-# SPECIALISE replicateA :: Int -> Maybe a -> Maybe [a] #-}
replicateA cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) f (loop (cnt - 1))
        
