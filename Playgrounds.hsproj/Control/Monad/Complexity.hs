{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Monad.Complexity where

import GHC.TypeLits
import Prelude hiding (Applicative(..), Monad(..), length)
import Prelude (fail)

newtype Cost (n :: Nat) a = Cost { unCost :: a }

(>>=) :: Cost n x -> (x -> Cost m y) -> Cost (n + m) y
(>>=) (Cost x) f = (Cost . unCost . f) x

return :: x -> Cost 0 x
return = Cost

single :: x -> Cost 1 x
single = Cost

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

length :: Vec n a -> Cost n Int
length Nil = return 0
length (Cons _ xs) = do
  tailLen <- length xs
  single (1 + tailLen)