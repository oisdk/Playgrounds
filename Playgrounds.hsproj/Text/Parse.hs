{-# language GeneralizedNewtypeDeriving, DeriveFunctor, LambdaCase #-}

module Text.Parse where
  
import Data.Foldable
import Prelude hiding (filter)
import Data.List hiding (filter)
import Data.Functor
import Control.Applicative

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
anyChar = Parser (maybeToList . uncons)
  
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

mapMaybe :: (a -> Maybe b) -> Parser a -> Parser b
mapMaybe f p = Parser $ \s -> [ (x,s) | (Just x,s) <- parse (fmap f p) s ]

filter :: (Alternative m, Monad m) => (a -> Bool) -> m a -> m a
filter p = (=<<) (\x -> if p x then pure x else empty)

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = filter p anyChar

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