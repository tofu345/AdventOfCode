module Main where

import Data.List
import Data.Maybe
import System.IO

weighting :: [(Char, Int)]
weighting =
    [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

pairs = [(')', '('), (']', '['), ('}', '{'), ('>', '<')]

scores :: [(Char, Int)]
scores = [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

main = do
    contents <- readFile "input.txt"
    partOne $ lines contents
    partTwo $ lines contents
    return ()

type PrevChunks = [Char]

corrupted :: PrevChunks -> String -> [Char]
corrupted _ [] = []
corrupted prevs (curr:rest)
  | curr `elem` "([{<" = corrupted (curr:prevs) rest
  | otherwise = let (prev:prevs') = prevs
                in if Just prev == lookup curr pairs
                   then corrupted prevs' rest
                   else curr : corrupted prevs' rest

partOne :: [String] -> IO ()
partOne contents = do
    putStr "Part One: "
    print . sum . mapMaybe (`lookup` weighting)
        $ concatMap (corrupted []) contents

incompletePairs :: PrevChunks -> String -> [Char]
incompletePairs prevs [] = prevs
incompletePairs prevs (curr:rest)
  | curr `elem` "([{<" = incompletePairs (curr:prevs) rest
  | otherwise = let (prev:prevs') = prevs
                in if Just prev == lookup curr pairs
                   then incompletePairs prevs' rest
                   else incompletePairs prevs rest

partTwo :: [String] -> IO ()
partTwo contents = do
    let points = map (mapMaybe (`lookup` scores) . incompletePairs [])
               $ filter ((==0) . length . corrupted [])
                   contents
    let totalPoints = sort
                    $ map (foldl (\acc v -> (acc * 5) + v) 0) points
    putStrLn $ "Part Two: " ++
             show (totalPoints !! (length totalPoints `div` 2))
