module Data.Para where
  
para :: (a -> b -> [a] -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x:xs) = f x (para f b xs) xs