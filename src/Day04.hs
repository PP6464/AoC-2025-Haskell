module Day04 where

import Data.Array
import Control.Monad.State

import Utils.Input
import Utils.List
import Utils.Grid

accessibleRoll :: Grid Char -> (Int, Int) -> Bool
accessibleRoll g (r, c) = g ! (r, c) == '@' && (4 > length (filter ( == '@') $ (g ! ) <$> neighbours (r, c) g))

remove :: Int -> Int -> State (Grid Char, Int) Int
remove h w = do
    (grid, tot) <- get

    let replacements = [((r,c), '.') | r <- 0..<h, c <- 0..<w, accessibleRoll grid (r,c)]

    if null replacements then
        return tot
    else do
        let grid' = grid // replacements

        put (grid', tot + length replacements)

        remove h w

main :: Bool -> IO ()
main test = do
    input <- selectInput 4 test
    let grid = parseGrid input
    let h = height grid
    let w = width grid
    putStrLn $ "Part 1: " ++ show (length [(0 :: Int) | r <- 0..<h, c <- 0..<w, accessibleRoll grid (r, c)])
    putStrLn $ "Part 2: " ++ show (evalState (remove h w) (grid, 0))