module Data.Coerce.Helpers where
  
import Data.Coerce

infixr 9 .#
(.#) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(.#) = const coerce

infixr 9 #.
(#.) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(#.) = const . coerce