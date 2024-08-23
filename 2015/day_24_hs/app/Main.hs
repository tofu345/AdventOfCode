module Main where

import Data.List
import Control.Monad
import Data.Function
import Data.Ord

main = do
    data' <- map read . lines <$> readFile "input.txt" :: IO [Int]

    putStrLn "Run time ~= 16secs :> (could be worse)"

    let highest = last data'
        g1 = group1 data' highest 3
    putStr "Part One: "
    print $ highest * (product . last $ g1)

    let g1' = group1 data' highest 4
        min' = minimumBy (compare `on` length) g1'
        ans = minimumBy (compare `on` product)
            $ filter (\v -> length min' == length v) g1'
    putStr "Part Two: "
    print $ product ans * highest

    where
    group1 data' highest groups =
        let requiredWeight = sum data' `div` groups
        in do
            v <- tail . subsequences $ init data'
            guard (sum v + highest == requiredWeight)
            return v
