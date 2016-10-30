{-# language RankNTypes #-}
{-# language DeriveFunctor #-}

module Control.Monad.Church.Source where
  
newtype List a =
  List (forall b. b -> (a -> List a -> b) -> b) deriving Functor
  
nil :: List a
nil = List const

cons :: a -> List a -> List a
cons x xs = List (\_ f -> f x xs)

instance Applicative List where
  pure x = cons x nil
  List fs <*> List xs =
    List (\b t -> fs b (\f ffs -> xs b (\x xxs -> t (f x) (ffs <*> xxs))))
    
instance Foldable List where
  foldr f b (List l) = l b (\x xs -> f x (foldr f b xs))
  
instance Show a => Show (List a) where
  show = show . foldr (:) []
  
