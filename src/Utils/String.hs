module Utils.String where

import Data.Text qualified as Text

trim :: String -> String
trim = (Text.unpack . Text.strip . Text.pack)

-- This splits a string by [\s]+
splitSpaces :: String -> [String]
splitSpaces text = reverse (reverse <$> splitSpaces' (trim text) [] True)
    where
        splitSpaces' :: String -> [String] -> Bool -> [String]
        splitSpaces' [] acc _ = acc
        splitSpaces' (' ':t) acc _ = splitSpaces' t acc True
        splitSpaces' (c:t) [] _ = splitSpaces' t [c:[]] False
        splitSpaces' (c:t) acc wasSpace
            | wasSpace = splitSpaces' t ([c] : acc) False
            | otherwise = splitSpaces' t ((c : head acc) : tail acc) False

