{-# language GeneralizedNewtypeDeriving, DeriveFunctor, LambdaCase #-}

module Text.Parse where
  
import Data.Foldable
import Data.Safe
import Data.List
import Data.Functor
import Control.Applicative
import Control.Applicative.Alternative

newtype Parser a =
  Parser { parse :: String -> [(a, String)] 
         } deriving (Functor, Monoid)
         
instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]
  Parser fs <*> Parser xs = Parser $ \s -> [ (f x, s) | (f,s) <- fs s, (x,s) <- xs s ]
  
instance Alternative Parser where
  empty = mempty
  Parser x <|> Parser y = Parser ((<|>) <$> x <*> y)
  
instance Monad Parser where
  x >>= f = Parser $ \s -> [ (y,s) | (x,s) <- parse x s, (y,s) <- parse (f x) s ]
  
anyChar :: Parser Char
anyChar = Parser (toAlt . uncons)

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = ensure p anyChar

oneOf :: String -> Parser Char
oneOf chrs = satisfies (`elem` chrs)

wspace :: Parser ()
wspace = (void . many . oneOf) " \t\n\r"

digit :: Parser Int
digit = flip mapMaybe anyChar $ \case
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  _ -> Nothing
  
natural :: Parser Int
natural = foldl' (\a e -> e + a * 10) 0 <$> many digit

chainl1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = rest =<< p where
  rest x = (do
    f <- op 
    y <- p
    rest (f x y)) <|> return x
                                



