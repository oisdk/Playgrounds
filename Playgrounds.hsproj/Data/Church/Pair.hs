{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Church.Pair where
  
import qualified Prelude
import Prelude ((.))
  
type Pair a b = forall c. (a -> b -> c) -> c

showP :: (Prelude.Show a, Prelude.Show b) => Pair a b -> Prelude.String
showP x = x (\y z -> "(" Prelude.++ Prelude.show y Prelude.++ "," Prelude.++ Prelude.show z Prelude.++ ")")
  
second :: (b -> c) -> Pair a b -> Pair a c
second f x c = x (\y -> c y . f)

first :: (a -> c) -> Pair a b -> Pair c b
first f x c = x (c . f)

fst :: Pair a b -> a
fst x = x (\y _ -> y)

snd :: Pair a b -> b
snd x = x (\_ y -> y)

pair :: a -> b -> Pair a b
pair x y c = c x y