{-# language GeneralizedNewtypeDeriving, DeriveFunctor, LambdaCase, RecursiveDo #-}

module Text.Parse where
  
import Data.Foldable
import Prelude hiding (head)
import Data.Safe
import Data.List hiding (head)
import Data.Functor
import Control.Applicative
import Control.Applicative.Alternative
import Control.Monad
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either
import Data.Maybe

newtype Parser a =
  Parser { parse :: String -> [(a, String)] 
         } deriving (Functor, Monoid)
         
instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]
  Parser fs <*> Parser xs = Parser $ \s -> [ (f x, s) | (f,s) <- fs s, (x,s) <- xs s ]
  
runParse :: Parser a -> String -> Maybe a
runParse p = fmap fst . head . parse p
  
instance Alternative Parser where
  empty = Parser (\_ -> [])
  Parser x <|> Parser y = Parser (\s -> x s ++ y s)
  
instance Monad Parser where
  x >>= f =
    Parser $ \s -> [ (y,s) 
                   | (x,s) <- parse x s
                   , (y,s) <- parse (f x) s ]

instance MonadPlus Parser

instance MonadFix Parser where
  mfix f = Parser (mfix . flip (parse . f . fst))
  
anyChar :: Parser Char
anyChar = Parser (toAlt . uncons)

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = afilter p anyChar

oneOf :: String -> Parser Char
oneOf chrs = satisfies (`elem` chrs)

lowercase :: Parser Char
lowercase = oneOf "abcdefghijklmnopqrstuvwxyz"

label :: Parser (Char, Int)
label = token (token (satisfies (':'==)) *> ((,) <$> token lowercase <*> token natural))

jump :: Map Char Int -> Parser (Maybe Int)
jump m = token $ do
  c <- token lowercase
  pure (Map.lookup c m)

lang :: Parser [Int]
lang = mdo
  (lbls, jmps) <- fmap partitionEithers (many (eitherA label (jump m)))
  let m = Map.fromList lbls
  return (catMaybes jmps)

wspace :: Parser ()
wspace = (void . many . oneOf) " \t\n\r"

digit :: Parser Int
digit = flip mapAlt anyChar $ \case
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
natural = foldl' (\a e -> e + a * 10) 0 <$> some digit

chainl1 :: (Monad m, Alternative m)
        => m a -> m (a -> a -> a) -> m a
chainl1 p op = rest =<< p where
  rest x = (do
    f <- op 
    y <- p
    rest (f x y)) <|> return x
                                
token :: Parser a -> Parser a
token p = p <* wspace

reserved :: String -> Parser ()
reserved v = token . Parser $ \s -> if v `isPrefixOf` s then [((),drop (length v) s)] else []

data Instruction
  = MovToM Reg AddrReg
  | MovFrM AddrReg Reg
  | Add Reg Reg
  deriving Show
  
data Reg = AL | BL deriving Show

newtype AddrReg = AddrReg { getReg :: Reg } deriving Show

reg :: Parser Reg
reg = AL <$ reserved "AL" <|> BL <$ reserved "BL"

addrReg :: Parser AddrReg
addrReg = AddrReg <$> (reserved "[" *> reg <* reserved "]")

