{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Control.Monad.Private where
  
import Control.Monad
import qualified Data.Map.Strict as Map
import System.Random
import Control.Arrow
import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans

newtype Private m d a = Private
  { _runPrivate :: ReaderT Double (MaybeT m) a
  } deriving Functor

--runPrivate :: Double -> (forall s. Private s m a) -> m a
--runPrivate x (Private xs) = xs x

instance Monad m => Applicative (Private m d) where
  pure = Private . pure
  Private fs <*> Private xs = Private (local (/2) fs <*> local (/2) xs)

instance Monad m => Monad (Private m d) where
  Private xs >>= f = Private (local (/2) xs >>= local (/2) . _runPrivate . f)

class Monad m => MonadRandom m where
  randDouble :: m Double
  
laplace :: MonadRandom m => Double -> m Double
laplace stddev = do
  uniform <- fmap (subtract 0.5) randDouble
  return (stddev * signum uniform * log(1 - 2.0 * abs uniform))
  
instance Monad m => Category (Private m) where
  id = Private . ReaderT . const . MaybeT . pure $ Nothing

--infixl 4 <+>
--(<+>) :: Applicative m => Private s m (a -> b) -> Private t m a -> Private u m b
--fs <+> xs = Private $ \s ->
--  _runPrivate fs (s/2) <*> _runPrivate xs (s/2)

anonymise :: MonadRandom m => m Double -> Private m d Double
anonymise value = Private (lift.lift =<< (reader $ \e -> (+) <$> value <*> laplace e))

--instance MonadRandom IO where
--  randDouble = getStdRandom random
--
--x :: Private s IO (Query s IO Double)
--x = makeQuery (pure 3)
--
--y :: Private s IO (Query s IO Double)
--y = makeQuery (pure 3)
--
--
--xs = do
--  a <- x
--  b <- y
--  c <- readQuery a
--  d <- readQuery b
--  return (c+d)
