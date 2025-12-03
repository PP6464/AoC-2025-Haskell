module Day03 where
import Utils.Input
import Data.Bifunctor (Bifunctor(second))
import Control.Monad.State

-- (max element, index)
maxWithIndex :: Ord a => [a] -> (a, Int)
maxWithIndex = second negate . maximum . flip zip [0,-1..]

dropLastN :: Int -> [a] -> [a]
dropLastN n = reverse . drop n . reverse

joltage :: Int -> [Int] -> Int
joltage n = evalState (joltage' 0) . (0,)
    where
        joltage' :: Int -> State (Int, [Int]) Int
        joltage' digs = do
            (j, bs) <- get

            if digs == n then
                return j
            else do
                let (m, i) = (maxWithIndex . dropLastN (n - 1 - digs)) bs
                put (j * 10 + m, drop (i + 1) bs)
                joltage' (digs + 1)

banks :: [String] -> [[Int]] 
banks = ((read @Int . (:[]) <$> ) <$> ) 

main :: Bool -> IO ()
main test = do
    input <- selectInput 3 test
    putStrLn $ "Part 1: " ++ show ((sum . map (joltage 2) . banks . lines) input)
    putStrLn $ "Part 2: " ++ show ((sum . map (joltage 12) . banks . lines) input)