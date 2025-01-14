module Main where

import Text.Regex.Posix
import Control.Monad
import Data.Foldable (foldl')
import Data.List

main :: IO ()
main = do
    data' <- map parse . lines <$> readFile "input.txt" :: IO [(Int, [Int])]
    let ops = [(+), (*)]

    putStr "Part One: "
    print $ sum $ map fst $ filter (uncurry $ valid ops) data'

    putStr "Part Two: "
    print $ sum $ map fst $ filter (uncurry $ valid $ concatInts : ops) data'

parse :: String -> (Int, [Int])
parse line =
    let [[_, target, nums]] = line =~ "([0-9]+): (.*)" :: [[String]]
     in (read target, read <$> words nums)

valid :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
valid ops target [num] = target == num
valid ops target (x1:x2:xs) = or $ [valid ops target (op x1 x2 : xs) | op <- ops]

concatInts :: Int -> Int -> Int
concatInts x y = let digits = floor (logBase 10 $ fromIntegral y) + 1
                  in x * (10 ^ digits) + y
