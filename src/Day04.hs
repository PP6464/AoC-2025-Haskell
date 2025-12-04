module Day04 where

import Data.Array
import Control.Monad.RWS

import Utils.Input
import Utils.List
import Utils.Grid

accessibleRoll :: Grid Char -> (Int, Int) -> Bool
accessibleRoll g (r, c) = g ! (r, c) == '@' && (4 > length (filter ( == '@') $ (g ! ) <$> neighbours (r, c) g))

remove :: RWS (Int, Int) String (Grid Char, Int) Int
remove = do
    (grid, tot) <- get
    (h, w) <- ask

    let n = count (accessibleRoll grid . fst) (assocs grid)

    if n == 0 then
        return tot
    else do
        let grid' = array ((0, 0), (h - 1, w - 1)) (replace grid <$> assocs grid)

        put (grid', tot + n)

        remove
    where
        replace :: Grid Char -> ((Int, Int), Char) -> ((Int, Int), Char)
        replace g (i, '.') = (i, '.')
        replace g (i, _) = if accessibleRoll g i then (i, '.') else (i, '@') 

main :: Bool -> IO ()
main test = do
    input <- selectInput 4 test
    let grid = parseGrid input
    putStrLn $ "Part 1: " ++ show (length $ filter (accessibleRoll grid . fst) (assocs grid))
    putStrLn $ "Part 2: " ++ show (fst $ evalRWS remove (height grid, width grid) (grid, 0))