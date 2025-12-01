module Day01 where

import Utils.Input (selectInput)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Control.Monad.State

dirns :: Map Char Int
dirns = Map.fromList [('L', -1), ('R', 1)]

type PWState = (Int, Int) -- (pwd, pos)

password1 :: [String] -> State PWState Int
password1 [] = gets fst
password1 ([]:_) = error "This is invalid input"
password1 ((d:step):ls) = do
    (pwd, pos) <- get

    let newPos = (pos + (dirns ! d) * read step) `mod` 100
    let newPwd = if newPos == 0 then pwd + 1 else pwd

    put (newPwd, newPos)

    password1 ls

password2 :: [String] -> State PWState Int
password2 [] = gets fst
password2 ([]:_) = error "This is invalid input"
password2 ((d:step):ls) = do
    (pwd, pos) <- get

    let (cycles, steps) = read step `quotRem` 100
    let newPosUnmod = pos + steps * (dirns ! d)
    let newPos = newPosUnmod `mod` 100
    let pwdInc = if (pos > 0 && newPosUnmod < 0) || (newPosUnmod > 99) || newPos == 0 then 1 else 0
    put (pwd + cycles + pwdInc, newPos)

    password2 ls


main :: Bool -> IO ()
main test = do
    input <- selectInput 1 test
    putStrLn $ "Part 1: " ++ show (evalState (password1 $ lines input) (0, 50))
    putStrLn $ "Part 2: " ++ show (evalState (password2 $ lines input) (0, 50))