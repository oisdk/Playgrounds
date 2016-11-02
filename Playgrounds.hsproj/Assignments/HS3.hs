module Assignments.HS3 where

import           Data.Char     (isLetter)
import           Data.Function (on)
import           Data.List     (groupBy, span)
import           Text.Read     (readMaybe)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

consonantTransform :: String -> String
consonantTransform word =
  let (consonants, rest) = span isConsonant word
  in rest ++ consonants ++ "ay"

vowelTransform :: String -> String
vowelTransform word = word ++ "way"

splitWords :: String -> [String]
splitWords = groupBy ((==) `on` isLetter)

rule1 :: String -> String
rule1 (c:cs) | isConsonant c = consonantTransform (c:cs)
             | otherwise = (c:cs)

rule2 :: String -> String
rule2 (c:cs) | isVowel c = vowelTransform (c:cs)
             | otherwise = (c:cs)

rule3 :: String -> String
rule3 (c:cs) | isConsonant c = consonantTransform (c:cs)
             | isVowel c = vowelTransform (c:cs)
             | otherwise = (c:cs)

main :: IO ()
main = do
  putStrLn "Mode 1, 2, or 3?"
  c <- getLine
  case readMaybe c of
    Just 1 -> interact (concatMap rule1 . splitWords)
    Just 2 -> interact (concatMap rule2 . splitWords)
    Just 3 -> interact (concatMap rule3 . splitWords)
    _ -> do
      putStr "Invalid mode: "
      print c
      main
