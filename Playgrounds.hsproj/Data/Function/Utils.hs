module Data.Function.Utils where
  
import Data.Bifunctor
import Control.Arrow ((***))

converge :: Eq a => (a -> a) -> a -> a
converge f = r where
  r x | x == y = y
      | otherwise = r y
      where y = f x
      
bicombine :: Bifunctor f => (ax -> bx -> cx) -> (ay -> by -> cy) -> (ax,ay) -> f bx by -> f cx cy
bicombine f g = uncurry bimap . (f *** g)