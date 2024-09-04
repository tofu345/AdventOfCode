module Main where

import Data.List

main = do
    contents <- lines <$> readFile "input.txt"
    let data' = map (map read . words) contents :: [[Int]]

    putStr "Part One: "
    print . length $ filter isPossible data'

    putStr "Part Two: "
    print . length . filter isPossible $ groupByColumns data' [] [] []

    where
    isPossible v =
        let [a, b, c] = sort v
         in a + b > c

    groupByColumns [] a b c = []
    groupByColumns ([x, y, z]:xs) a b c =
        let a' = x : a
            b' = y : b
            c' = z : c
         in if length a == 2
            then a' : b' : c' : groupByColumns xs [] [] []
            else groupByColumns xs a' b' c'
