{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List
import Data.Bool
  
-- Exercise 1
skips :: [a] -> [[a]]
skips xs = [[ e | (i, e) <- zip (cycle [1..n]) xs, i == n] | n <- [1..length xs]]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:rest@(y:z:_))
    | y > x && y > z = y : localMaxima rest
    | otherwise          = localMaxima rest
localMaxima _ = []

-- Exercise 3
base :: String
base = replicate 10 '=' ++ "\n0123456789\n"

listToString :: [Integer] -> String
listToString xs = map (\x -> bool ' ' '*' (elem x xs)) [0..9] ++ "\n\n"

histogram :: [Integer] -> String
histogram = (++ base) . concat . map listToString . reverse . transpose . group . sort