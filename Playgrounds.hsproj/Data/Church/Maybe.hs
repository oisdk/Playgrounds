{-# LANGUAGE RankNTypes #-}

module Data.Church.Maybe where
  
import Prelude ((.), id, flip)
import qualified Prelude

type Maybe a = forall b. b -> (a -> b) -> b

just :: a -> Maybe a
just x _ f = f x

nothing :: Maybe a
nothing x _ = x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f m b c = m b (c . f)

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe fs xs b c = fs b (\f -> xs b (c . f))

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe xs f b c = xs b (\x -> f x b c)

altMaybe :: Maybe a -> Maybe a -> Maybe a
altMaybe xs ys = xs ys just

