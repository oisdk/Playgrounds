{-# language DeriveFunctor #-}

module Data.PList where

import Control.Monad hiding (filterM)
import Data.Monoid
import Control.Applicative
import Control.Applicative.Alternative
import Control.Monad.ListT hiding (traverse, head)
import Control.Monad.IO.Class
import Control.Monad.Identity hiding (filterM)
import Control.Monad.Trans.Class
import Prelude hiding (drop, null, take)

data PList m a = P { unO :: ListT m (ListT m a) } deriving Functor

headm :: (Functor f, Monoid m) => ListT f m -> f m
headm (ListT xs) = maybe mempty fst <$> xs

tailm :: (Monad f) => ListT f m -> ListT f m
tailm = drop 1

zipm :: (Monad f, Monoid m) => ListT f (ListT f m) -> ListT f m
zipm ms = let
  heads :: (Monoid m, Functor f) => ListT f (ListT f m) -> ListT f (f m)
  heads = fmap headm
  tails :: Monad f => ListT f (ListT f m) -> ListT f (ListT f m)
  tails = fmap tailm
  h = fold (fmap . mappend) mempty (heads ms)
  t = zipm (filterM (fmap not . null) (tails ms))
  in ListT $ do
    alln <- fold (\a e -> (a&&) <$> null e) True  ms
    if alln then pure Nothing else fmap (\x -> Just (x,t)) h

instance Monad m => Applicative (PList m) where
  (<*>) = ap
  pure = P . pure . pure

instance Monad m => Monad (PList m) where
  x >>= f = let P xs = (fmap (unO . f) x) in P (ListT $ join =<< uncons xs) where
    join Nothing = pure Nothing
    join (Just ~(m,ms)) = do
      let part1 = zipm m
      let part2 = ListT $ join =<< (uncons ms)
      head' <- headm part1
      return $ Just (head', zipm (fromFoldable [tailm part1, part2]))

instance Monad m => Alternative (PList m) where
   empty = P empty
   P xs <|> P ys = P (zipm (fromFoldable [xs,ys]))

instance MonadTrans PList where
  lift =
    P . pure . ListT . liftM (\a -> Just (a, mempty))

instance MonadIO m => MonadIO (PList m) where
  liftIO =
    lift . liftIO

ex1 :: PList Identity (Int,Int,Int)
ex1 = do
  x <- P $ fmap pure (fromFoldable [1..])
  y <- P $ fmap pure (fromFoldable [1..])
  z <- P $ fmap pure (fromFoldable [1..])
  guard $ x*x+y*y==z*z
  return $ (x,y,z)
  
toList' :: Monad m => PList m a -> m [a]
toList' (P xs) = toList xs >>= (fmap join . traverse toList)