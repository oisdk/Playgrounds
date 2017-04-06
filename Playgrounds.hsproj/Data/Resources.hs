module Data.Resources (dict) where
  
import Data.ByteString (ByteString, hGetLine)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO hiding (hGetLine)
import Control.Monad.State.Strict as State
import Data.Bool
import System.IO.Unsafe

ioDict :: IO (Set ByteString)
ioDict = flip State.execStateT Set.empty $ do
  h <- liftIO (openFile "dict.txt" ReadMode)
  whileM_ (liftIO (not <$> hIsEOF h)) $ do
    word <- liftIO (hGetLine h)
    modify (Set.insert word)
  
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = p >>= bool (pure ()) (f *> go)
    
dict :: Set ByteString
dict = unsafePerformIO ioDict