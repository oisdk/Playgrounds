{-# LANGUAGE DeriveFunctor #-}

module Data.Semiring.Map where
  
import qualified Data.Map.Strict as Map
import Data.Semiring hiding (add)
import Data.Semiring.Free
import Prelude hiding (lookup)
import Data.Foldable hiding (toList)
import Control.Applicative
import Data.Monoid
import Control.Monad

newtype SemiringMap a b = SemiringMap
  { getMap :: Map.Map a (Free b)
  } deriving (Functor, Show)

type Map a b      = SemiringMap a (First b)
type Set a        = SemiringMap a (Add Bool)
type MultiMap a b = SemiringMap a [b]
type MultiSet a   = SemiringMap a (Add Int)

lookup :: (Ord a, Monoid b) => a -> SemiringMap a b -> b
lookup x (SemiringMap xs) = foldMap fold (Map.lookup x xs)

add :: (Ord a, Semiring b) => a -> SemiringMap a b -> SemiringMap a b
add x (SemiringMap xs) = SemiringMap (Map.insertWith (<+>) x (pure one) xs)

insert :: Ord a => a -> b -> SemiringMap a b -> SemiringMap a b
insert k v (SemiringMap xs) = SemiringMap (Map.insertWith mappend k (pure v) xs)

instance Ord a => Monoid (SemiringMap a b) where
  mempty = SemiringMap Map.empty
  mappend (SemiringMap xs) (SemiringMap ys) = SemiringMap (Map.unionWith mappend xs ys)

delete :: Ord a => a -> SemiringMap a b -> SemiringMap a b
delete x (SemiringMap xs) = SemiringMap (Map.delete x xs)

intersect :: Ord a => SemiringMap a b -> SemiringMap a b -> SemiringMap a b
intersect (SemiringMap xs) (SemiringMap ys) = SemiringMap (Map.intersectionWith (<.>) xs ys)

fromList :: (Ord a, Semiring b) => [a] -> SemiringMap a b
fromList = foldr add mempty

fromAssocs :: (Ord a, Monoid b) => [(a,b)] -> SemiringMap a b
fromAssocs = foldr (uncurry insert) mempty

toList :: (Semiring b, Enum b) => SemiringMap a b -> [a]
toList = uncurry (flip replicate) <=< (Map.assocs . fmap (fromEnum.unFree) . getMap)

toAssocs :: Monoid b => SemiringMap a b -> [(a,b)]
toAssocs = Map.assocs . fmap fold . getMap