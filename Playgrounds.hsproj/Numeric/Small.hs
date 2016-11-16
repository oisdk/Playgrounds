{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.Small where
  
import Data.Monoid
import Data.Bits
import Data.Proxy
import Data.Int
import Test.SmallCheck.Series
import Data.Coerce
import GHC.TypeLits
import Data.Word
import Data.Function
import Data.Ord

data Size = O | S Size

newtype WordN (n :: Size) = WordN 
  { getWord :: Integer }

newtype IntN (n :: Size) = IntN 
  { getInt :: Integer }

class Coercible n Integer => BitAlias n where
  mask  :: n -> Integer
  trunc :: n -> n

coerceBinary :: Coercible a b => (a -> a -> a) -> b -> b -> b
coerceBinary = coerce

coerceUnary :: Coercible a b => (a -> a) -> b -> b
coerceUnary = coerce

convBinary :: BitAlias n => (Integer -> Integer -> Integer) -> n -> n -> n
convBinary f = trunc .: coerceBinary f

(.:) = (.).(.)

convUnary :: BitAlias n => (Integer -> Integer) -> n -> n
convUnary f = trunc . coerceUnary f

instance BitAlias (WordN O) where
  mask _ = 0
  {-# INLINE mask #-}
  trunc x = coerceUnary (mask x .&.) x
  {-# INLINE trunc #-}
  
instance BitAlias (WordN s) => BitAlias (WordN (S s)) where
  mask x = smask x undefined where
    smask :: BitAlias (WordN s) => WordN (S s) -> WordN s -> Integer
    smask _ y = setBit (shiftL (mask y) 1) 0
    {-# INLINE smask #-}
  {-# INLINE mask #-}
  trunc x = coerceUnary (mask x .&.) x
  {-# INLINE trunc #-}
  
instance BitAlias (IntN O) where
  mask _ = 0
  {-# INLINE mask #-}
  trunc x = coerceUnary (mask x .&.) x
  {-# INLINE trunc #-}

instance BitAlias (IntN s) => BitAlias (IntN (S s)) where
  mask x = smask x undefined where
    smask :: BitAlias (IntN s) => IntN (S s) -> IntN s -> Integer
    smask _ y = setBit (shiftL (mask y) 1) 0
    {-# INLINE smask #-}
  {-# INLINE mask #-}
  trunc x = coerceUnary (mask x .&.) x
  {-# INLINE trunc #-}

instance Bounded (WordN O) where
  minBound = WordN 0
  {-# INLINE minBound #-}
  maxBound = WordN 1
  {-# INLINE maxBound #-}
  
instance BitAlias (WordN b) => Bounded (WordN (S b)) where
  minBound = WordN 0
  {-# INLINE minBound #-}
  maxBound = x where x = WordN (mask x)
  {-# INLINE maxBound #-}

instance Num (WordN O) where
  (+) = convBinary (+)
  {-# INLINE (+) #-}
  (*) = convBinary (*)
  {-# INLINE (*) #-}
  (-) = convBinary (-)
  {-# INLINE (-) #-}
  abs = convUnary abs
  {-# INLINE abs #-}
  signum = convUnary signum
  {-# INLINE signum #-}
  fromInteger = trunc . WordN
  {-# INLINE fromInteger #-}

instance BitAlias (WordN s) => Num (WordN (S s)) where
  (+) = convBinary (+)
  {-# INLINE (+) #-}
  (*) = convBinary (*)
  {-# INLINE (*) #-}
  (-) = convBinary (-)
  {-# INLINE (-) #-}
  abs = convUnary abs
  {-# INLINE abs #-}
  signum = convUnary signum
  {-# INLINE signum #-}
  fromInteger = trunc . WordN
  {-# INLINE fromInteger #-}

instance Enum (WordN O) where
  toEnum = trunc . WordN . toEnum
  fromEnum = fromEnum . getWord

instance BitAlias (WordN s) => Enum (WordN (S s)) where
  toEnum = trunc . WordN . toEnum
  fromEnum = fromEnum . getWord

instance Show (WordN s) where
  show = show . getWord

instance Show (IntN s) where
  show = show . getInt
  
instance Eq (WordN O) where
  (==) = (==) `on` getWord . trunc
  
instance BitAlias (WordN s) => Eq (WordN (S s)) where
  (==) = (==) `on` getWord . trunc
  
instance Ord (WordN O) where
  compare = comparing (getWord.trunc)
  
instance BitAlias (WordN s) => Ord (WordN (S s)) where
  compare = comparing (getWord.trunc)
  
instance Monad m => Serial m (WordN O) where
  series = generate $ \d -> take d [minBound..maxBound]
  
instance (Monad m, BitAlias (WordN s)) => Serial m (WordN (S s)) where
  series = generate $ \d -> take d [minBound..maxBound]

type family WordS (n :: Nat) :: *
type instance WordS 1 = WordN O
type instance WordS 2 = WordN (S (S O))
type instance WordS 3 = WordN (S (S (S O)))
type instance WordS 4 = WordN (S (S (S (S O))))
type instance WordS 5 = WordN (S (S (S (S (S O)))))
type instance WordS 6 = WordN (S (S (S (S (S (S O))))))
type instance WordS 7 = WordN (S (S (S (S (S (S (S O)))))))

type Word1 = WordS 1
type Word2 = WordS 2
type Word3 = WordS 3
type Word4 = WordS 4
type Word5 = WordS 5
type Word6 = WordS 6
type Word7 = WordS 7

type family IntS (n :: Nat) :: *
type instance IntS 1 = IntN O
type instance IntS 2 = IntN (S (S O))
type instance IntS 3 = IntN (S (S (S O)))
type instance IntS 4 = IntN (S (S (S (S O))))
type instance IntS 5 = IntN (S (S (S (S (S O)))))
type instance IntS 6 = IntN (S (S (S (S (S (S O))))))
type instance IntS 7 = IntN (S (S (S (S (S (S (S O)))))))


type Int1 = IntS 1
type Int2 = IntS 2
type Int3 = IntS 3
type Int4 = IntS 4
type Int5 = IntS 5
type Int6 = IntS 6
type Int7 = IntS 7



