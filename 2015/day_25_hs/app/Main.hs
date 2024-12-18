module Main where

import Data.List

idx row col = sum [1..col + row - 2] + col - 1

calc :: Int -> Int
calc = (`mod` 33554393) . (* 252533)

main = do
    putStr "Part One: "
    print $ iterate' calc 20151125 !! idx 2978 3083
