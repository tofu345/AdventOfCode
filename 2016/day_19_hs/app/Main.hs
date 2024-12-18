module Main where

import Data.Char
import Control.Monad (forM_)

main :: IO ()
main = do
    let dec = toBin 3004953
        -- consulted the solutions :<
        winner = fromBin (tail dec ++ [head dec]) -- https://youtu.be/uCsD3ZGzMgE?si=UY0w8tHWAE-TDt2a&t=716
    -- putStrLn $ "Part One: " ++ show winner

    let input = 3004953
        samples = [1..100]
        out = (\n -> (n, p2 [1..n])) <$> samples
    forM_ out $ \(n, w) -> putStrLn $ show n ++ " | " ++ show w

-- https://stackoverflow.com/a/9166342
toBin :: Int -> String
toBin 0 = "0"
toBin n = reverse (helper n)
    where
    helper 0 = []
    helper n | n `mod` 2 == 1 = '1' : helper (n `div` 2)
             | otherwise = '0' : helper (n `div` 2)

fromBin :: String -> Int
fromBin [] = 0
fromBin [ch] = digitToInt ch
fromBin chs = let len = length chs
               in fromBin' $ zip [len, len - 1..0] chs
    where
    fromBin' :: [(Int, Char)] -> Int
    fromBin' [] = 0
    fromBin' ((i, c) : cs) =
        let powOf2 = 2 ^ (i - 1)
         in (digitToInt c * powOf2) + fromBin' cs

p2 :: [Int] -> Int
p2 [x] = x
p2 ns = p2' ns (length ns) 0
    where
    p2' :: [Int] -> Int -> Int -> Int
    p2' [x] _ _ = x
    p2' ns len idx
        | idx >= len = p2' ns len 0
        | otherwise =
            let mid = max 1 (len `div` 2)
                (x, y) = splitAt (midPointFrom idx len) ns
             in p2' (x ++ tail y) (len - 1) (idx + 1)

midPointFrom :: Int -> Int -> Int
midPointFrom cur len =
    let idx = cur `mod` len
        mid = len `div` 2
     in case compare idx mid of
            LT -> idx + mid
            GT -> idx - (mid + len `rem` 2)
            EQ -> 0
