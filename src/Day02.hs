module Day02 where

import Utils.Input (selectInput)
import Data.List.Split

ranges :: String -> [Int]
ranges str = concatMap (\range -> let [s, e] = splitOn "-" range in [read @Int s .. read @Int e]) $ splitOn "," str

invalid1 :: Int -> Bool
invalid1 s
    | (even . length) (show s) = s `mod` (10 ^ power + 1) == 0
    | otherwise = False
    where
        power = length (show s) `div` 2

-- Gives factors that aren't 1
factors :: Int -> [Int]
factors x = filter (\f -> x `mod` f == 0) [2..x]

invalid2 :: Int -> Bool
invalid2 s = any (\m -> s `mod` m == 0) mods
    where
        l = (length . show) s
        mods = map (read @Int . (\x -> concat $ replicate x (replicate (l `div` x - 1) '0' ++ "1"))) (factors l)

main :: Bool -> IO ()
main test = do
    input <- selectInput 2 test
    putStrLn $ "Part 1: " ++ show ((sum . filter invalid1 . ranges) input)
    putStrLn $ "Part 2: " ++ show ((sum . filter invalid2 . ranges) input)