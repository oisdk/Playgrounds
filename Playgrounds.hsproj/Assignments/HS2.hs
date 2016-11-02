module Assignments.HS2 where
  
sort :: Ord a => [a] -> [a]
sort xs = sort' (length xs) xs where
  sort' n xs
    | n <= 1 = xs
    | otherwise = merge (sort' m ys) (sort' (n-m) zs)
    where
      ys = take m xs
      zs = drop m xs
      m = n `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = case compare x y of
  LT -> x : merge xs (y:ys)
  EQ -> x : y : merge xs ys
  GT -> y : merge (x:xs) ys