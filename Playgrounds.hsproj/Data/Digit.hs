{-# language LambdaCase #-}
{-# language BangPatterns #-}

module Data.Digit where
  
import Data.Monoid
import Data.Tuple
import Data.List
import Test.QuickCheck hiding (Positive)
  
data Digit = Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           deriving (Eq, Ord, Show, Enum, Bounded)
           
data Sign = Negative | Positive deriving (Eq, Ord, Show)

data Whole = 
  Whole { sign :: Sign
        , digs :: [Digit] 
        } deriving (Eq)
        
instance Show Whole where
  show (Whole s d) = case s of
    Negative -> '-' : rest
    Positive -> rest
    where rest = flip map (reverse d) $ \case
            Zero  -> '0'
            One   -> '1'
            Two   -> '2'
            Three -> '3'
            Four  -> '4'
            Five  -> '5'
            Six   -> '6'
            Seven -> '7'
            Eight -> '8'
            Nine  -> '9'
            
instance Ord Whole where
  compare (Whole _ []) (Whole _ []) = EQ
  compare (Whole Negative _) (Whole Positive _) = LT
  compare (Whole Positive _) (Whole Negative _) = GT
  compare (Whole Negative x) (Whole Negative y) =
    compare (Whole Positive y) (Whole Positive x)
  compare (Whole Positive d) (Whole Positive e) =
    cmp EQ d e where
      cmp !ac [] [] = ac
      cmp !ac (x:xs) (y:ys) = 
        cmp (compare x y <> ac) xs ys
      cmp _ [] _  = LT
      cmp _  _ [] = GT
      
instance Arbitrary Whole where
  arbitrary = fmap fromInteger arbitrary

instance Num Whole where
  fromInteger n
    | n < 0 = Whole Negative (fromPos (negate n))
    | otherwise = Whole Positive (fromPos n)
    where 
      fromPos = map toDig . unfoldr qr
      qr 0 = Nothing
      qr n = Just (swap (quotRem n 10))
      toDig = toEnum . fromInteger




