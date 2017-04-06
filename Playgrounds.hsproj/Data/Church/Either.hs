{-# LANGUAGE RankNTypes #-}

module Data.Church.Either where
  
import Prelude (id, (.), flip)
  
type Either a b = forall c. (a -> c) -> (b -> c) -> c

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f e l r = e l (r . f)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f e l r = e (l . f) r

left :: a -> Either a b
left x l _ = l x

right :: b -> Either a b
right x _ r = r x

apEither :: Either a (b -> c) -> Either a b -> Either a c
apEither fs xs l r = fs l (\f -> xs l (r . f))

bindEither :: Either a b -> (b -> Either a c) -> Either a c
bindEither e c l r = e l (\x -> c x l r)