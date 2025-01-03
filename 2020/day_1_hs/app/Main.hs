module Main where

import Control.Monad (guard)

main :: IO ()
main = do
    nums <- map read . lines <$> readFile "input.txt" :: IO [Int]

    putStr "Part One "
    print $ head $ do
            a <- nums
            b <- nums
            guard (a + b == 2020)
            return (a * b)

    putStr "Part Two "
    print $ head $ do
            a <- nums
            b <- nums
            c <- nums
            guard (a + b + c == 2020)
            return (a * b * c)
