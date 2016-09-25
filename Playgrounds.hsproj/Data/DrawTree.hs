{-# language TemplateHaskell, LambdaCase #-}

module Data.DrawTree where
  
import Control.Lens hiding (children)
import Control.Monad.State
import Data.IntMap
import Data.Maybe

type Id = Int

data TreeState a = TreeState
  { _curId :: Id
  , _objs  :: IntMap (DrawTree a) }

data DrawTree a = DrawTree 
  { _x :: Double
  , _y :: Int
  , _content :: a
  , _children :: [Id]
  , _parent :: Maybe Id
  , _thread :: Maybe Id
  , _offset :: Int
  , _ancestor :: Maybe Id
  , _change :: Int
  , _shift :: Int
  , _lmost_sibling :: Maybe Id
  , _number :: Int }

makeLenses ''TreeState
makeLenses ''DrawTree

type ErrorState a = StateT (TreeState a) (Either String)

newId :: ErrorState a Id
newId = do
  i <- use curId
  curId += 1
  pure i
  
dup i = error ("Duplicate trees at id " ++ show i)
missing i = error ("No tree at id " ++ show i)

viewTree :: Id -> ErrorState a (DrawTree a)
viewTree i = use (objs . at i) >>= (maybe (missing i) pure)

putTree :: Id -> DrawTree a -> ErrorState a ()
putTree i t = use (objs . at i) >>= \case
  Nothing -> objs . at i ?= t
  Just _ -> dup i
  
modifyTree :: Id -> (DrawTree a -> DrawTree a) -> ErrorState a ()
modifyTree i f = do
  t <- viewTree i
  objs . at i ?= f t


lastMay, headMay :: [a] -> Maybe a
lastMay [x] = Just x
lastMay (x:xs) = lastMay xs
lastMay [] = Nothing

headMay (x:_) = Just x
headMay _ = Nothing

leftBrother :: Id -> ErrorState a (Maybe Id)
leftBrother i = do
  self <- viewTree i
  case self ^. parent of 
    Nothing -> pure Nothing
    Just p -> do
      prnt <- viewTree p
      pure (lastMay . takeWhile (i/=) $ prnt ^. children)

isLmostSibling :: Id -> ErrorState a Bool
isLmostSibling i = do
  t <- viewTree i
  case view parent t of
    Nothing -> pure False
    Just p -> do
      prnt <- viewTree p
      pure (Just i == headMay (prnt ^. children))
      
getLmostSibling :: Id -> ErrorState a (Maybe Id)
getLmostSibling i = do
  t <- viewTree i
  islm <- isLmostSibling i
  case (not islm, t^.lmost_sibling, t^.parent) of
    (True, Nothing, Just p) -> do
      prnt <- viewTree p
      case headMay (prnt^.children) of
        Nothing -> error $ "No children of parent tree " ++ show p
        Just c -> modifyTree i (lmost_sibling ?~ c)
    _ -> pure ()
  nt <- viewTree i
  pure (nt ^. lmost_sibling)
  



