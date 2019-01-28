module Lib ( dotProduct, isUnique, isAscending, to ) where

dotProduct :: (Num a) => (a, a) -> (a, a) -> a
dotProduct (ax, ay) (bx, by) = (ax * bx) + (ay * by)

--vecLength :: (Num a) => (a, a) -> Float
--vecLength (x, y) = sqrt ((fromIntegral x) * (fromIntegral y))

isUnique :: (Eq a) => [a] -> Bool
isUnique (head:tail) = do
  if length tail == 0
    then True
    else
      if elem head tail
        then False
        else
          isUnique tail

isAscending :: (Ord a) => [a] -> Bool
isAscending [] = True
isAscending (x:[]) = True
isAscending (x:y:[]) = x <= y
isAscending (x:xs) = (isAscending xs) && (x <= head xs)

to :: (Enum a) => a -> a -> [a]
to start end = [start..end]
