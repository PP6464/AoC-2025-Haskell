module Utils.List where

(..<) :: Int -> Int -> [Int]
a ..< b = [a..(b - 1)]

(...) :: Int -> Int -> [Int]
a ... b = [a..b]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n xs = take n xs : slices n (drop n xs)

replace :: Eq a => a -> a -> [a] -> [a]
replace old new = fmap (\x -> if x == old then new else x) 