module Main where

import Control.Monad
import Control.Applicative
import System.Exit

presents = 29000000

main :: IO ()
main = do
    let ans1 =  partOne 1
    putStr "Part One: "
    print ans1

    let ans2 = partTwo ans1
    putStr "Part Two: "
    print ans2

-- https://www.rookieslab.com/posts/most-efficient-way-to-find-all-factors-of-a-number-python-cpp
factors :: (Integral a) => a -> [a]
factors n = concatMap
    (\v -> do
        guard (n `rem` v == 0)
        v : do guard (n `div` v /= v) >> [n `div` v])
    [1..floor (sqrt (fromIntegral n))]

partOne :: Int -> Int
partOne n = do
    if sum (map (*10) (factors n)) >= presents
        then n
        else partOne (n + 1)

partTwo :: Int -> Int
partTwo num = do
    let cutoff = num `div` 50
    let f = filter (>= cutoff) $ factors num
    if sum (map (*11) f) >= presents
        then num
        else partTwo (num + 1)
