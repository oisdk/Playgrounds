module Data.Traversable.Extras where
  
import Data.Bool
import Control.Applicative
import Control.Monad hiding (filterM)
import Control.Monad.State hiding (filterM)
import qualified Data.Set as Set
import Data.Traversable

filterM :: (Monad m, Traversable m, Alternative m, Applicative f)
        => (a -> f Bool)
        -> m a
        -> f (m a)
filterM p = fmap join . traverse (\x -> (<$) x . guard <$> p x)

ordNub :: (Monad m, Traversable m, Alternative m, Ord a)
       => m a -> m a
ordNub = flip evalState Set.empty
       . filterM (\e -> gets (Set.notMember e) <* modify' (Set.insert e))

ordNubOn :: (Ord b, Traversable f, Monad f, Alternative f) => (a -> b) -> f a -> f a
ordNubOn k = flip evalState Set.empty
       . filterM (\e -> let m = k e in gets (Set.notMember m) <* modify' (Set.insert m))
       
--unique :: (Ord a, Traversable f) => f a -> Bool
--unique = and . snd . mapAccumL (\a e -> (Set.insert e a, Set.notMember e a)) Set.empty

unique :: (Ord a, Foldable f) => f a -> Bool
unique xs = foldr f (const True) xs Set.empty where
  f e a s = Set.notMember e s && a (Set.insert e s)
  
