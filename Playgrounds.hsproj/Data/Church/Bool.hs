{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Church.Bool where
  
import Prelude (const, (.), id, flip)
import qualified Prelude

  
type Bool = forall a. a -> a -> a
newtype WrappedBool = WrapBool { unwrapBool :: Bool }

true, false :: Bool
true = const
false = const id

showB :: Bool -> Prelude.String
showB b = b "true" "false"

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) x y t f = x (y t f) f

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) x y t = x t . y t

not :: Bool -> Bool
not = flip
