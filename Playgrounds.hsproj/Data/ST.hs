module Data.ST where
  
import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Data.Array.ST

type NumArray s = STUArray s Int Int

prime :: Int -> [Int]
prime n = runST $ do
  arr <- newListArray (0,n) [0..]
  