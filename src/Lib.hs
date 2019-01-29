module Lib ( dotProduct, vecLength, isUnique, isAscending, to ) where

dotProduct :: (Num a) => (a, a) -> (a, a) -> a
dotProduct (ax, ay) (bx, by) = (ax * bx) + (ay * by)

vecLength :: (Floating a) => (a, a) -> a
vecLength (x, y) = sqrt (x * x + y * y)

isUnique :: (Eq a) => [a] -> Bool
isUnique [] = True
isUnique [x] = True
isUnique [x, y] = x /= y
isUnique (x:xs) = not (elem x xs) && (isUnique xs)

isAscending :: (Ord a) => [a] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending [x, y] = x <= y
isAscending (x:xs) = (isAscending xs) && (x <= head xs)

to :: (Enum a, Ord a) => a -> a -> [a]
to start end = if (start < end) then [start..end] else reverse [end..start]
