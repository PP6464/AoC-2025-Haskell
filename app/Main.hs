module Main where

import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayStr, "-ti"] -> runDay (read dayStr) True
        [dayStr]        -> runDay (read dayStr) False
        _               -> putStrLn "Usage: haskell-aoc2025 <day> <?-ti>"

runDay :: Int -> Bool -> IO ()
runDay 1 = Day01.main
runDay 2 = Day02.main
runDay 3 = Day03.main
runDay 4 = Day04.main
runDay 5 = Day05.main
runDay 6 = Day06.main
runDay 7 = Day07.main
runDay 8 = Day08.main
runDay 9 = Day09.main
runDay n = \_ -> putStrLn ("Day " ++ show n ++ " not implemented.")
