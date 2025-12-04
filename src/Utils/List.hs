module Utils.List where

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f