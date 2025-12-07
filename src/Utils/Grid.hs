module Utils.Grid where

import Data.Array

import Utils.List

type Grid a = Array (Int, Int) a

height :: Grid a -> Int
height = succ . fst . snd . bounds

width :: Grid a -> Int
width = succ . snd . snd . bounds

-- Gets the neighbouring indices for a given index
neighbours :: (Int, Int) -> Grid a -> [(Int, Int)]
neighbours (r, c) g = filter (\(x, y) -> x >= 0 && x < h && y >= 0 && y < w && (x /= r || y /= c)) (liftA2 (,) [r - 1, r, r + 1] [c - 1, c, c + 1])
    where
        h = height g
        w = width g

parseGrid :: String -> Grid Char
parseGrid input = array ((0, 0), (h - 1, w - 1)) $ zip [(row, col) | row <- 0..<h, col <- 0..<w] (concat ls)
    where
        ls = lines input
        h = length ls
        w = (length . head) ls