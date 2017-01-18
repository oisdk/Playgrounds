import Text.Read
import Text.ParserCombinators.ReadP
import Control.Applicative

prse :: Read a => ReadP a
prse = readS_to_P reads

rp :: ReadP a -> String -> Maybe a
rp r s = case readP_to_S r s of
  [(x,_)] -> Just x
  _ -> Nothing


p :: ReadP (Integer, Double, String, Bool)
p = (,,,) <$> prse
          <*> prse
          <*> prse
          <*> prse

trip :: a -> b -> c -> (a,b,c)
trip x y z = (x,y,z)