module Assignments.HS3 where
  
import Data.List (span)
  
vowels :: String
vowels = "aeiouAEIOU"

transformOne :: String -> String
transformOne xs =
  let (ys,zs) = span (`notElem` vowels) xs
  in zs ++ ys ++ "ay"
  
transformTwo :: String -> String
transformTwo = (++"way")

isRuleTwo :: String -> Bool
isRuleTwo (x:xs) = x `elem` vowels
isRuleTwo _ = False

isRuleOne :: String -> Bool
isRuleOne = not . isRuleTwo

both :: String -> String
both xs | isRuleOne xs = transformOne xs
        | otherwise = transformTwo xs