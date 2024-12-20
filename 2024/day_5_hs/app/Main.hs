module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (partition, delete, find)
import Data.List.Split (splitOn, wordsBy)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let (r, u) = break null contents
        rules = M.fromListWith (++) (map parse r)
        upds = reverse . map read . wordsBy (== ',') <$> tail u :: [[Int]]
    run rules upds

parse :: String -> (Int, [Int])
parse s = let [l, r] = splitOn "|" s in (read l, [read r])

run :: Map Int [Int] -> [[Int]] -> IO ()
run rules upds = do
    let (correct, incorrect) = partition inOrder upds

    putStr "Part One: "
    print $ sum (middle <$> correct)

    putStr "Part Two: "
    print $ sum (middle . fix' <$> incorrect)
    where
    middle :: [Int] -> Int
    middle ns = ns !! (length ns `div` 2)

    inOrder :: [Int] -> Bool
    inOrder (n:ns) | Just rls <- M.lookup n rules =
                       all (`notElem` rls) ns && inOrder ns
                   | otherwise = inOrder ns
    inOrder [] = True

    swap :: Eq a => a -> a -> [a] -> [a]
    swap _ _ [] = []
    swap x y (ch:str)
        | ch `elem` [x, y] = head (delete ch [x, y]) : swap x y str
        | otherwise = ch : swap x y str

    fix' :: [Int] -> [Int]
    fix' nums | inOrder nums = nums
              | otherwise = f nums
        where
        f :: [Int] -> [Int]
        f [] = nums
        f (n:ns)
            | Just rls <- M.lookup n rules =
                let other = find (`elem` ns) rls
                 in case other of Nothing -> f ns
                                  Just o -> fix' (swap n o nums)
            | otherwise = f ns
