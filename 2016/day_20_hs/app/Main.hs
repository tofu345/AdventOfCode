module Main where

import Data.List.Split

main :: IO ()
main = do
    blacklist <- map parse . lines <$> readFile "input.txt"
    let ips = solve (0, 4294967295) blacklist
    putStrLn $ "Part One: " ++ show (head ips)
    putStrLn $ "Part Two: " ++ show (length ips)

parse :: String -> (Int, Int)
parse str =
    let x = splitOn "-" str
     in case x of [l, h] -> (read l, read h)
                  _ -> error "invalid data"

solve :: (Int, Int) -> [(Int, Int)] -> [Int]
solve (start, end) blacklist = solve' start blacklist
    where
    solve' :: Int -> [(Int, Int)] -> [Int]
    solve' n [] = n : solve' (n + 1) blacklist
    solve' n ((lo, hi):lims)
        | n > end = []
        | n >= lo && n <= hi = solve' (hi + 1) blacklist
        | otherwise = solve' n lims
