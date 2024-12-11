module Main where

import Data.List

main :: IO ()
main = do
    reports <- map (map read . words) . lines
            <$> readFile "input.txt" :: IO [[Int]]

    putStr "Part One: "
    print (length $ filter safe reports)

    putStr "Part Two: "
    print (length $ filter safe2 reports)

safe :: [Int] -> Bool
safe vs@(a:b:_) =
    let dir = compare a b
        adjSafe (x:y:_) = compare x y == dir && safeDiff x y
        adjSafe _ = True
     in all adjSafe (tails vs)
     where
     safeDiff a b = let d = abs (a - b)
                     in d >= 1 && d <= 3
safe _ = True

safe2 :: [Int] -> Bool
safe2 vs = let dropped1 = [delete' i vs | i <- [0..length vs - 1]]
            in safe vs || any safe dropped1

type Index = Int
delete' :: Index -> [Int] -> [Int]
delete' i vs 
    | i < 0 || i >= length vs = error "invalid index"
    | otherwise = take i vs ++ drop (i + 1) vs
