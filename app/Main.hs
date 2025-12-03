module Main where

import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayStr, "-ti"] -> runDay (read dayStr) True
        [dayStr]       -> runDay (read dayStr) False
        _              -> putStrLn "Usage: haskell-aoc2025 <day> <?-ti>"

runDay :: Int -> Bool -> IO ()
runDay 1 test = Day01.main test
runDay 2 test = Day02.main test
runDay 3 test = Day03.main test
runDay n _ = putStrLn $ "Day " ++ show n ++ " not implemented."
