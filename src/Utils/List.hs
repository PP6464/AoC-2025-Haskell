module Utils.List where

(..<) :: Int -> Int -> [Int]
a ..< b = [a..(b - 1)]

(...) :: Int -> Int -> [Int]
a ... b = [a..b]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f