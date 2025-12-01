module Utils.Input (selectInput) where

readInput :: Int -> IO String
readInput n = readFile $ "inputs/day" ++ pad n ++ ".txt"
  where
    pad k = if k < 10 then '0' : show k else show k

readTestInput :: Int -> IO String
readTestInput n = readFile $ "inputs/day" ++ pad n ++ "_test.txt"
  where
    pad k = if k < 10 then '0' : show k else show k

selectInput :: Int -> Bool -> IO String
selectInput n False = readInput n
selectInput n True  = readTestInput n
