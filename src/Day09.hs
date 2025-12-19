module Day09 where

import Utils.Input

import Data.List.Split
import Data.List
import Control.Monad.State
import Data.List.Extra (snoc)

type Point = (Int, Int)
data Region = UL | UM | UR | ML | MM | MR | BL | BM | BR deriving (Eq, Show)

opposite :: Region -> Region
opposite UL = BR
opposite UM = BM
opposite UR = BL
opposite ML = MR
opposite MM = MM
opposite MR = ML
opposite BL = UR
opposite BM = UM
opposite BR = UL

largestArea1 :: [Point] -> Int
largestArea1 pts = maximum [(abs (fst x - fst y) + 1) * (abs (snd x - snd y) + 1) | (x:ys) <- tails pts, y <- ys]

largestArea2 :: [Point] -> Int
largestArea2 pts = maximum [(abs (fst x - fst y) + 1) * (abs (snd x - snd y) + 1) | (x:ys) <- tails pts, y <- ys, rectInLoop x y pts]

rectInLoop :: Point -> Point -> [Point] -> Bool
rectInLoop (_, _) (_, _) [] = True
rectInLoop (x1, y1) (x2, y2) (pt:pts) = evalState travelPts (region pt, pts `snoc` pt)
    where
        xl = min x1 x2
        xh = max x1 x2
        yl = min y1 y2
        yh = max y1 y2
        isLeft = ( <= xl) . fst
        isRight = ( >= xh) . fst
        isUp = ( <= yl) . snd
        isDown = ( >= yh) . snd
        travelPts :: State (Region, [Point]) Bool
        travelPts = do
            (reg, ps) <- get

            if null ps then do
                return True
            else do
                let (point:points) = ps
                let r = region point
                if r == MM || r == opposite reg then do
                    return False
                else do
                    put (r, points)
                    travelPts
        region p
            | isLeft p && isUp p = UL
            | isLeft p && isDown p = BL
            | isLeft p = ML
            | isRight p && isUp p = UR
            | isRight p && isDown p = BR
            | isRight p = MR
            | isUp p = UM
            | isDown p = BM
            | otherwise = MM

parseInput :: String -> [Point]
parseInput input = (\l -> let [x, y] = splitOn "," l in (read x, read y)) <$> lines input

main :: Bool -> IO ()
main test = do
    input <- selectInput 9 test
    let pts = parseInput input
    putStrLn $ "Part 1: " ++ show (largestArea1 pts)
    putStrLn $ "Part 2: " ++ show (largestArea2 pts)