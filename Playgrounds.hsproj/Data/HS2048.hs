{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}

module Data.HS2048 where
  
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad.Reader
import Control.Arrow
import Data.Function.Operators
import Control.Monad.State

padTo :: Int -> [a] -> [Maybe a]
padTo = flip (foldr f (flip replicate Nothing)) where
  f e a n = Just e : a (n-1)

data GameConfig = GameConfig
  { _gameConfigBase :: Int }
  
makeFields ''GameConfig

squish :: Int
       -> [Maybe Int]
       -> [Maybe Int]
squish b = catMaybes
     >>> group
     >>> map (head &&& length)
     >>> traverse (uncurry (comb b))
     >>> concat 
     >>> padTo (b*b)
  where
    comb b e l = let (q,r) = quotRem l b in replicate q (e*b) ++ replicate r e
    sqr x = x * x
    
data GameState = GameState
  { _gameStateBoard :: [[Maybe Int]] 
  } deriving Show
  
makeFields ''GameState

move :: (MonadReader r m, HasBase r Int, MonadState s m)
     => Setter' s [Maybe Int] -> m ()
move targ = do
  b <- view base
  targ %= squish b

moveLeft, moveRight, moveUp, moveDown ::
    (MonadReader r m, HasBase r Int, MonadState s m, HasBoard s [[Maybe Int]])
     => m ()
moveLeft  = move (board . each)
moveRight = move (board . each . reversed)
moveUp    = move (board . transposed . each)
moveDown  = move (board . transposed . each . reversed)

transposed :: Iso [[a]] [[b]] [[a]] [[b]]
transposed = iso (transposeOf each) (transposeOf each)
  
type Env = ReaderT GameConfig (State GameState)

