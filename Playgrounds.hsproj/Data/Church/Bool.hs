{-# LANGUAGE RankNTypes #-}

module Data.Church.Bool where
  
import Prelude hiding (Bool(..), not, (&&), (||))
import qualified Prelude as NonChurch

newtype Bool = B { fi :: forall a. a -> a -> a }

instance Show Bool where
  show b = fi b "True" "False"
  
instance Eq Bool where
  x == y = 
    fi x 
      (fi y NonChurch.True NonChurch.False)
      (fi y NonChurch.False NonChurch.True)
      
instance Ord Bool where
  compare x y =
    fi x (fi y EQ GT) (fi y LT EQ)
    
true, false :: Bool
true  = B (\t _ -> t)
false = B (\_ f -> f)

not :: Bool -> Bool
not x = B (\t f -> fi x f t)

(&&) :: Bool -> Bool -> Bool
(&&) x y = B (\t f -> fi x (fi y t f) f)

(||) :: Bool -> Bool -> Bool
(||) x y = B (\t -> fi x t . fi y t)