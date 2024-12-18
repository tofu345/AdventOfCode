module Main where

import Data.List.Split
import Data.Bifunctor

main = do
    data' <- map parse . lines <$> readFile "input.txt"

    let p1 = filter
            (\(sup, hyp) -> not (any abba hyp) && any abba sup)
            data'
    putStr "Part One: "
    print $ length p1

    let p2 = foldl f [] data'
    putStr "Part Two: "
    print $ length p2

    where
    f acc (sup, hyp) = 
        let a = concatMap aba sup
            b = concatMap aba hyp
            v = filter (\v -> reverse v `elem` b) a
         in if not (null v)
                then v : acc
                else acc

parse :: String -> ([String], [String])
parse l = foldl f ([], []) $ splitOn "[" l
    where
    f acc v
        | ']' `elem` v =
            let [hyp, sup] = splitOn "]" v
             in bimap (sup :) (hyp :) acc
        | otherwise = first (v :) acc

abba xs@(a:b:b':a':_) =
    (a /= b && a == a' && b == b') || abba (tail xs)
abba _ = False

aba xs@(a:b:a':_) =
    if a /= b && a == a'
        then [a,b] : aba (tail xs)
        else aba (tail xs)
aba _ = []
