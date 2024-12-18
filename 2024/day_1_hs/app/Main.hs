module Main where

import Data.List

main :: IO ()
main = do
    lists <- map (map read . words) . lines
          <$> readFile "input.txt" :: IO [[Int]]
    let l1 = map head lists
        l2 = map last lists

    putStr "Part One: "
    print (p1 l1 l2)

    putStr "Part Two: "
    print (p2 l1 l2)

p1 :: [Int] -> [Int] -> Int
p1 = p1' 0
    where
    p1' dist [a] [b] = dist + abs (a - b)
    p1' dist l1 l2 =
        let a = minimum l1
            b = minimum l2
            newDist = dist + abs (a - b)
         in p1' newDist (delete a l1) (delete b l2)

p2 :: [Int] -> [Int] -> Int
p2 l1 l2 = foldl' (f l2) 0 l1
    where
    f :: [Int] -> Int -> Int -> Int
    f l2 acc v = acc + v * length (filter (== v) l2)
