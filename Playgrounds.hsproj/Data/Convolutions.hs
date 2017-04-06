{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Data.Convolutions where
  
import Data.Semiring
import Data.Maybe 
import Test.QuickCheck

--convolve :: (a -> b -> c) -> [a] -> [b] -> [c]
--convolve f xs ys = foldr walk base ys (const []) where
--  base k = k xs
--  walk x xs k = xs $ \case
--    (y:ys) -> f y x : k ys
--    [] -> []

--convolve :: (a -> b -> c) -> [a] -> [b] -> [c]
--convolve f = convFold (\x y zs -> f x y : zs) []
--    
--convFold :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
--convFold f b xs ys = foldr walk base ys (const b) where
--  base k = k xs
--  walk x xs k = xs $ \case
--    (y:ys) -> f y x (k ys)
--    [] -> b

convFold :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
convFold f b xs ys = foldr walk base xs (\_ r -> r) where
  base k = k ys b
  walk x xs k = xs $ \case
    y:ys -> \r -> k ys (f x y r)
  
convolve :: (a -> b -> c) -> [a] -> [b] -> [c]
convolve f = convFold (\x y zs -> f x y : zs) []


suffixes f xs@(_:xxs) ys@(_:yys) = convolve f xs ys : suffixes f xxs yys
suffixes _ _ _ = []

prefixes f xs ys = foldr walk base xs (\_ r -> r) where
  base k = k ys [] : []
  walk x xs k = k ys [] : (xs $ \case
    y:ys -> \r -> k ys (f x y : r)
    [] -> \r -> r)
  
isPalindrome xs = walk xs xs (\[] -> True) where
  walk xs1 [] k = k xs1
  walk (_ : xs1) [_] k = k xs1
  walk (x:xs1) (_:_:xs2) k = walk xs1 xs2 (\(y:ys) -> x == y && k ys)
  

mul' :: Semiring a => [a] -> [a] -> [a]
mul' [] [] = []
mul' [] _ = []
mul' _ [] = []
mul' xs ys = map add (prefixes (<.>) xs ys) ++ map add (suffixes (<.>) xs ys)

instance Semiring a =>
         Semiring [a] where
    one = [one]
    zero = []
    [] <+> ys = ys
    xs <+> [] = xs
    (x:xs) <+> (y:ys) = (x <+> y) : (xs <+> ys)
    [] <.> _ = []
    _ <.> [] = []
    (x:xs) <.> (y:ys) =
        (x <.> y) : (map (x <.>) ys <+> map (<.> y) xs <+> (xs <.> ys))