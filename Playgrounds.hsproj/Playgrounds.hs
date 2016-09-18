import Control.Monad.Writer
import Data.List
import Data.Foldable
import Control.Lens

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lt ++ [x] ++ qsort gt where
  (lt,gt) = partition (<x) xs
