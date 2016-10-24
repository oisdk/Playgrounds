module Assignments.HS2 where
  
first :: (a -> c) -> (a,b) -> (c,b)
first f ~(x,y) = (f x,y)

second :: (b -> c) -> (a,b) -> (a,c)
second f ~(x,y) = (x,f y)

splitEven :: [a] -> ([a],[a])
splitEven xs = foldr f (const ([],[])) xs True where
  f e a b = (if b then first else second) (e:) (a (not b))
  
sort :: Ord a => [a] -> [a]
--sort [] = []
--sort [x] = [x]
--sort xs = let (ys,zs) = splitEven xs in merge (sort ys) (sort zs)
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