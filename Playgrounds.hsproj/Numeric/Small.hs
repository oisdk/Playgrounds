{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Numeric.Small where
  
import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Numeric.Natural

newtype IntOfSize (n :: Nat) = IntOfSize
    { getIntOfSize :: Integer
    } deriving (Generic)

instance KnownNat n =>
         Bounded (IntOfSize n) where
    minBound = let n = IntOfSize (shift (-1) (fromInteger (natVal (Proxy :: Proxy n) - 1))) in n
    maxBound =  IntOfSize (shift 1 (fromInteger (natVal (Proxy :: Proxy n) - 1)) - 1)

type CoerceBinary a b = (a -> a -> a) -> (b -> b -> b)

instance KnownNat n =>
         Bits (IntOfSize n) where
    (.&.) = (coerce :: CoerceBinary Integer (IntOfSize n)) (.&.)
    (.|.) = (coerce :: CoerceBinary Integer (IntOfSize n)) (.|.)
    xor = trunc .: (coerce :: CoerceBinary Integer (IntOfSize n)) xor
    complement =
        trunc . (coerce :: (Integer -> Integer) -> IntOfSize n -> IntOfSize n) complement
    shift =
        trunc .:
        (coerce :: (Integer -> Int -> Integer) -> IntOfSize n -> Int -> IntOfSize n)
            shift
    rotate =
        trunc .:
        (coerce :: (Integer -> Int -> Integer) -> IntOfSize n -> Int -> IntOfSize n)
            rotate
    bit = trunc . IntOfSize . bit
    bitSize = fromInteger . natVal
    bitSizeMaybe = Just . fromInteger . natVal
    isSigned _ = True
    testBit =
        (coerce :: (Integer -> Int -> Bool) -> IntOfSize n -> Int -> Bool)
            testBit
    popCount =
        (coerce :: (Integer -> Int) -> IntOfSize n -> Int) popCount

trunc
    :: KnownNat n
    => IntOfSize n -> IntOfSize n
trunc x
  | testBit x (fromInteger (natVal x) - 1) = x .|. minBound
  | otherwise = x .&. maxBound

convBinary
    :: KnownNat n
    => CoerceBinary Integer (IntOfSize n)
convBinary f = trunc .: coerce f

instance KnownNat n =>
         Num (IntOfSize n) where
    (+) = convBinary (+)
    (*) = convBinary (*)
    negate y = complement y + 1
    fromInteger = trunc . IntOfSize . fromInteger
    abs = id
    signum (IntOfSize x) = IntOfSize (signum x)

instance KnownNat n =>
         Eq (IntOfSize n) where
    (==) = (==) `on` getIntOfSize . trunc

instance KnownNat n =>
         Ord (IntOfSize n) where
    compare = compare `on` getIntOfSize . trunc

instance KnownNat n =>
         Real (IntOfSize n) where
    toRational = toRational . getIntOfSize

instance KnownNat n =>
         Enum (IntOfSize n) where
    fromEnum = fromEnum . getIntOfSize
    toEnum = trunc . IntOfSize . toEnum
    enumFrom x = [x .. maxBound]

instance KnownNat n =>
         Integral (IntOfSize n) where
    toInteger = toInteger . getIntOfSize
    quotRem x y = (convBinary quot x y, convBinary rem x y)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

instance KnownNat n =>
         FiniteBits (IntOfSize n) where
    finiteBitSize = fromInteger . natVal

allIntsOfSize
    :: KnownNat n
    => [IntOfSize n]
allIntsOfSize = f [0 .. maxBound ] (drop 1 [0,-1 .. minBound])
  where
    f (x:xs) ys = x : f ys xs
    f [] ys = ys

instance KnownNat n =>
         Show (IntOfSize n) where
    showsPrec n = showsPrec n . getIntOfSize . trunc

newtype WordOfSize (n :: Nat) = WordOfSize
    { getWordOfSize :: Natural
    } deriving (Generic)

instance KnownNat n =>
         Bounded (WordOfSize n) where
    minBound = WordOfSize 0
    maxBound = WordOfSize (shift 1 (fromInteger (natVal (Proxy :: Proxy n))) - 1)

instance KnownNat n =>
         Bits (WordOfSize n) where
    (.&.) = (coerce :: CoerceBinary Natural (WordOfSize n)) (.&.)
    (.|.) = (coerce :: CoerceBinary Natural (WordOfSize n)) (.|.)
    xor = truncW .: (coerce :: CoerceBinary Natural (WordOfSize n)) xor
    complement =
        truncW . (coerce :: (Natural -> Natural) -> WordOfSize n -> WordOfSize n) complement
    shift =
        truncW .:
        (coerce :: (Natural -> Int -> Natural) -> WordOfSize n -> Int -> WordOfSize n)
            shift
    rotate =
        truncW .:
        (coerce :: (Natural -> Int -> Natural) -> WordOfSize n -> Int -> WordOfSize n)
            rotate
    bit = truncW . WordOfSize . bit
    bitSize = fromInteger . natVal
    bitSizeMaybe = Just . fromInteger . natVal
    isSigned _ = False
    testBit =
        (coerce :: (Natural -> Int -> Bool) -> WordOfSize n -> Int -> Bool)
            testBit
    popCount =
        (coerce :: (Natural -> Int) -> WordOfSize n -> Int) popCount

truncW
    :: KnownNat n
    => WordOfSize n -> WordOfSize n
truncW = (.&.) maxBound

convBinaryW
    :: KnownNat n
    => CoerceBinary Natural (WordOfSize n)
convBinaryW f = truncW .: coerce f

instance KnownNat n =>
         Num (WordOfSize n) where
    (+) = convBinaryW (+)
    (*) = convBinaryW (*)
    negate y = (maxBound `xor` y) + 1
    fromInteger = truncW . WordOfSize . fromInteger
    abs = id
    signum (WordOfSize x) = WordOfSize (signum x)

instance KnownNat n =>
         Eq (WordOfSize n) where
    (==) = (==) `on` getWordOfSize . truncW

instance KnownNat n =>
         Show (WordOfSize n) where
    showsPrec n = showsPrec n . getWordOfSize . truncW

instance KnownNat n =>
         Ord (WordOfSize n) where
    compare = compare `on` getWordOfSize . truncW

instance KnownNat n =>
         Real (WordOfSize n) where
    toRational = toRational . getWordOfSize

instance KnownNat n =>
         Enum (WordOfSize n) where
    fromEnum = fromEnum . getWordOfSize
    toEnum = truncW . WordOfSize . toEnum
    enumFrom x = [x .. maxBound]

instance KnownNat n =>
         Integral (WordOfSize n) where
    toInteger = toInteger . getWordOfSize
    quotRem x y = (convBinaryW quot x y, convBinaryW rem x y)


instance KnownNat n =>
         FiniteBits (WordOfSize n) where
    finiteBitSize = fromInteger . natVal

allWordsOfSize
    :: KnownNat n
    => [WordOfSize n]
allWordsOfSize = [minBound .. maxBound]