module Data.Traversable.Extras where
  
import Data.Bool
import Control.Applicative
import Control.Monad hiding (filterM)
import Control.Monad.State hiding (filterM)
import qualified Data.Set as Set

filterM :: (Monad m, Traversable m, Alternative m, Applicative f)
        => (a -> f Bool)
        -> m a
        -> f (m a)
filterM p = fmap join . traverse (\x -> (<$) x . guard <$> p x)

ordNub :: (Monad m, Traversable m, Alternative m, Ord a)
       => m a -> m a
ordNub = flip evalState Set.empty
       . filterM (\e -> gets (Set.notMember e) <* modify' (Set.insert e))