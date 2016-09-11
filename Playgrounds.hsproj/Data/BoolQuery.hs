module Data.BoolQuery where
  
import Prelude hiding (not, (&&), (||))
import qualified Data.Bool as Bool

class BoolQuery a where
  
  infixr 3 &&
  (&&) :: a -> a -> a
  
  infixr 2 ||
  (||) :: a -> a -> a
  
  infixr 5 `nand`
  nand :: a -> a -> a
  
  not :: a -> a

  x && y = let z = nand x y in nand z z
  x || y = nand x x `nand` nand y y
  not x = nand x x
  nand x y = not (x && y)

instance BoolQuery Bool where
  False && _ = False
  True  && x = x
  True  || _ = True
  False || x = x
  not True   = False
  not False  = True
  
instance BoolQuery b => BoolQuery (a -> b) where
  not f x    = not (f x)
  (f || g) x = f x || g x
  (f && g) x = f x && g x
  nand f g x = f x `nand` g x