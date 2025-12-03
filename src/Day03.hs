module Day03 where
import Utils.Input
import Data.Bifunctor (Bifunctor(second))
import Control.Monad.State

-- (max element, index)
maxWithIndex :: [Int] -> (Int, Int)
maxWithIndex l = second negate $ maxWithIndex' l 0
    where
        maxWithIndex' :: [Int] -> Int -> (Int, Int)
        maxWithIndex' [] _ = error "Impossible to choose maximum"
        maxWithIndex' [x] i = (x, -i)
        maxWithIndex' (x:xs) i = max (x, -i) (maxWithIndex' xs (i + 1))

dropLastN :: Int -> [a] -> [a]
dropLastN n = reverse . drop n . reverse

joltage1 :: [Int] -> Int
joltage1 = evalState joltage' . (0,)
    where
        joltage' :: State (Int, [Int]) Int
        joltage' = do
            (j, bs) <- get
            let numDigits = if j == 0 then 0 else (length . show) j

            if numDigits == 2 then
                return j
            else do
                let (m, i) = (maxWithIndex . dropLastN (1 - numDigits)) bs
                put (j * 10 + m, drop (i + 1) bs)
                joltage'

joltage2 :: [Int] -> Int
joltage2 = evalState joltage' . (0,)
    where
        joltage' :: State (Int, [Int]) Int
        joltage' = do
            (j, bs) <- get
            let numDigits = if j == 0 then 0 else (length . show) j

            if numDigits == 12 then
                return j
            else do
                let (m, i) = (maxWithIndex . dropLastN (11 - numDigits)) bs
                put (j * 10 + m, drop (i + 1) bs)
                joltage'

banks :: [String] -> [[Int]] 
banks = map (map (read @Int . (:[])))

main :: Bool -> IO ()
main test = do
    input <- selectInput 3 test
    putStrLn $ "Part 1: " ++ show ((sum . map joltage1 . banks . lines) input)
    putStrLn $ "Part 2: " ++ show ((sum . map joltage2 . banks . lines) input)