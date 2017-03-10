module Debug where

import System.IO.Unsafe

{-# WARNING trace "'trace' remains in code" #-}
trace :: Show a => a -> a
trace expr = unsafePerformIO (do
    print expr
    pure expr)

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = error "Not implemented"