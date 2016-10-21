{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators, DataKinds, UndecidableInstances, ScopedTypeVariables #-}

module Lib where

import System.Random

pick :: (Bounded a, Enum a) => IO a
pick = fmap (toEnum @a) $ randomRIO (fromEnum (minBound @a), fromEnum (maxBound @a))
