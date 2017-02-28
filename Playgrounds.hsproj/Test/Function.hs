{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module Test.Function where
  
import qualified Data.IntMap as IntMap
import  Data.IntMap (IntMap)
import Control.Arrow hiding ((<+>), (<.>))
import Data.Semiring
import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Monoid
import Control.Lens hiding ((<.>))

-- | A representation of a function
data Func a b = Func b (IntMap b)
  deriving (Eq, Ord)
  
func :: (Enum a, Bounded a, Ord b) => Iso' (Func a b) (a -> b)
func = iso apply fromFunc

newtype EndoFunc a = EndoFunc (Endo a)

instance (Enum a, Bounded a, Ord a) => Eq (EndoFunc a) where
  EndoFunc (Endo f) == EndoFunc (Endo g) = fromFunc f == fromFunc g

instance (Enum a, Bounded a, Ord a, Show a) => Show (EndoFunc a) where
  show (EndoFunc (Endo f)) = show (fromFunc f)

fromList' :: Eq b => b -> [(Int,b)] -> Func a b
fromList' cnst
  = Func cnst
  . IntMap.fromList
  . filter ((cnst/=) . snd)

fromList :: (Enum a, Eq b) => b -> [(a,b)] -> Func a b
fromList cnst
  = fromList' cnst
  . map (first fromEnum)

fromFunc :: (Enum a, Bounded a, Ord b) => (a -> b) -> Func a b
fromFunc f = fromList cnst (zip xs ys) where
  xs = [minBound..maxBound]
  ys = map f xs
  Just cnst = mostFrequent ys

eFromFunc :: (a -> a) -> EndoFunc (Add a)
eFromFunc f = (EndoFunc . Endo) (Add . f . getAdd)

mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = fmap fst . fst . foldl' f (Nothing, Map.empty :: Map.Map a Int) where
  f (b,m) e = (Just nb, Map.insert e c m) where
    c = maybe 1 succ (Map.lookup e m)
    nb = case b of
      Just (a,d) | d >= c -> (a,d)
      _          -> (e,c)

apply :: Enum a => Func a b -> a -> b
apply (Func c cs) x = IntMap.findWithDefault c (fromEnum x) cs

instance (Enum a, Show a, Show b) => Show (Func a b) where
  showsPrec _ (Func c xs :: Func a b)  = showChar '{' . IntMap.foldrWithKey f b xs where
    f x y a = shows (toEnum x :: a) . showString " -> " . shows y . showString ", " . a
    b = showString "_ -> " . shows c . showChar '}'
    
instance (Enum a, Bounded a, Ord b, Semiring b) => Semiring (Func a b) where
  zero = zero ^. from func
  one  = zero ^. from func
  x <+> y = ((x ^. func) <+> (y ^. func)) ^. from func
  x <.> y = ((x ^. func) <.> (y ^. func)) ^. from func