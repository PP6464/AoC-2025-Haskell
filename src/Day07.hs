module Day07 where

import Utils.Input
import Utils.Grid

import Data.Array
import Data.Set qualified as Set
import Data.STRef
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Data.Map qualified as Map
import Control.Monad.ST
import Control.Monad

part1 :: Grid Char -> Int
part1 grid = runST $ do
    let n = height grid
    row <- newSTRef 1
    splits <- newSTRef 0
    beams <- newSTRef (Set.fromList [y | (0, y) <- indices grid, grid ! (0, y) == 'S'])

    let step = do
            r <- readSTRef row
            b <- readSTRef beams

            let splitters = Set.fromList [y | (i, y) <- indices grid, i == 2 * r, grid ! (i, y) == '^']
            let splitPos = Set.intersection splitters b
            modifySTRef' splits ( + Set.size splitPos)

            forM_ (Set.toList splitPos) $ \x ->
                modifySTRef' beams $ \bs ->
                    Set.insert (x+1) (Set.insert (x-1) (Set.delete x bs))

            modifySTRef' row succ

    replicateM_ (n `div` 2 - 1) step

    readSTRef splits

part2 :: Grid Char -> Int
part2 grid = runST $ do
    let n = height grid
    row <- newSTRef 1
    beams <- newSTRef (MS.fromList [y | (0, y) <- indices grid, grid ! (0, y) == 'S'])
    tmp <- newSTRef MS.empty

    let step = do
            r <- readSTRef row
            bs <- readSTRef beams

            let splitters = Set.fromList [y | (i, y) <- indices grid, i == 2 * r, grid ! (i, y) == '^']

            let bsList = MS.toOccurList bs

            forM_ bsList $ \(x, k) -> do
                    if x `elem` splitters then do
                        modifySTRef' tmp (MS.insertMany (x - 1) k) 
                        modifySTRef' tmp (MS.insertMany (x + 1) k)
                    else do
                        modifySTRef' tmp (MS.insertMany x k)

            t <- readSTRef tmp

            writeSTRef beams t

            writeSTRef tmp MS.empty
            modifySTRef' row succ

    replicateM_ (n `div` 2 - 1) step

    bs <- readSTRef beams

    pure (length bs)

main :: Bool -> IO ()
main test = do
    input <- selectInput 7 test
    let grid = parseGrid input
    putStrLn $ "Part 1: " ++ show (part1 grid)
    putStrLn $ "Part 2: " ++ show (part2 grid)