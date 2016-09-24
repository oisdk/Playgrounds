import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lt ++ [x] ++ qsort gt where
  (lt,gt) = partition (<x) xs
