module Main where

import Control.Monad (guard)

main :: IO ()
main = do
    nums <- map read . lines <$> readFile "input.txt" :: IO [Int]

    let p1 = do
            a <- nums
            b <- nums
            guard (a + b == 2020)
            return (a * b)

    let p2 = do
            a <- nums
            b <- nums
            c <- nums
            guard (a + b + c == 2020)
            return (a * b * c)

    putStr "Part One "
    print (head p1)

    putStr "Part Two "
    print (head p2)
