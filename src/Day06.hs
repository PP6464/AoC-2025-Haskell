module Day06 where

import Utils.Input
import Utils.List
import Utils.String

import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as Map

data Problem = Problem {
    nums :: [Int],
    op :: Int -> Int -> Int
}

calcProb :: Problem -> Int
calcProb p = foldr1 (op p) (nums p)

ops :: Map String (Int -> Int -> Int)
ops = Map.fromList [("*", (*)), ("+", (+))]

problems :: [String] -> [[String]]
problems chunks = reverse <$> problems' chunks [] True
    where
        problems' :: [String] -> [[String]] -> Bool -> [[String]]
        problems' [] acc _ = acc
        problems' (str:strs) acc wasSpace
            | str == replicate n ' ' = problems' strs acc True
            | wasSpace = problems' strs ([str] : acc) False
            | otherwise = problems' strs ((str : head acc) : tail acc) False
        n = (length . head) chunks

parseProblem1 :: [String] -> Problem
parseProblem1 ps = Problem { nums = read <$> init ps, op = ops ! last ps }

parseProblem2 :: [String] -> Problem
parseProblem2 ps = Problem { nums = nums, op = op }
    where
        nums = read . init <$> ps
        op = ((ops ! ) . (:[]) . last . head) ps

parseInput1 :: String -> [Problem]
parseInput1 input = parseProblem1 <$> transpose (splitSpaces <$> lines input)

parseInput2 :: String -> [Problem]
parseInput2 input = map parseProblem2 (problems chunks)
    where
        chunks = (transpose . lines) input


main :: Bool -> IO ()
main test = do
    input <- selectInput 6 test
    putStrLn $ "Part 1: " ++ show (sum $ calcProb <$> parseInput1 input)
    putStrLn $ "Part 2: " ++ show (sum $ calcProb <$> parseInput2 input)