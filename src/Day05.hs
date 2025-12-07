module Day05 where

import Utils.Input
import Utils.List

import Data.Bifunctor
import Data.List.Split
import Control.Monad.State
import Data.List

parseInput :: [String] -> ([(Int, Int)], [Int])
parseInput input = bimap (map parseRange) (map read) (parseInput' input False [] [])
    where
        parseInput' [] _ f s = (f, s)
        parseInput' ("":is) False f s = parseInput' is True f s
        parseInput' (i:is) b f s
            | b = parseInput' is b f (i : s)
            | otherwise = parseInput' is b (i : f) s

-- turns "a-b" into (a, b)
parseRange :: String -> (Int, Int)
parseRange x = let [f, s] = splitOn "-" x in (read f, read s)

inRange :: (Int, Int) -> Int -> Bool
inRange (l, h) x = l <= x && x <= h

addRange :: (Int, Int) -> State (Int, Int) ()
addRange (l, h) = modify (\(m, tot) -> (max m h, tot + max 0 (min (h + 1 - l) (h - m)))) 

rangesSize :: [(Int, Int)] -> Int
rangesSize r = snd $ execState (mapM_ addRange (sort r)) (0, 0)

main :: Bool -> IO ()
main test = do
    input <- selectInput 5 test
    let (fresh, ingredients) = (parseInput . lines) input
    putStrLn $ "Part 1: " ++ show (count (\x -> any (`inRange` x) fresh) ingredients)
    putStrLn $ "Part 2: " ++ show (rangesSize fresh)