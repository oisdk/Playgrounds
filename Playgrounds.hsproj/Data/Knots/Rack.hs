-- | https://en.wikipedia.org/wiki/Racks_and_quandles

module Data.Knots.Rack where
  
-- | Laws:
-- a <| (b <| c) = (a <| b) <| (a <| c)
-- (c |> b) |> a = (c >| a) |> (b |> a)
-- (a <| b) |> a = b
-- a <| (b |> a) = b
class Rack a where
  (<|) :: a -> a -> a
  (|>) :: a -> a -> a
  

-- | a |> a = a
class Rack a => Quandle a

