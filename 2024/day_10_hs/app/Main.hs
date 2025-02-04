{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!), (!?))
import Data.Foldable (foldl')
import Data.List

type Pos = (Int, Int)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let pmap = M.fromList [ ((x, y), ch) | (y, line) <- zip [0..] (lines contents)
                                         , (x, ch) <- zip [0..] line ] :: Map Pos Char
    putStr "Part One: "
    print $ p1 pmap

    putStr "Part One: "
    print $ p2 pmap

adjacent (x, y) = [ (x, y - 1)
                  , (x, y + 1)
                  , (x - 1, y)
                  , (x + 1, y)
                  ]

-- would have preferred combining the two parts, but oh well

p1 :: Map Pos Char -> Int
p1 pmap = M.foldlWithKey' start 0 pmap
    where
    f cur acc = foldl' (flip (check cur)) acc $ adjacent cur

    start acc pos '0' = acc + length (f pos [])
    start acc _ _ = acc

    check :: Pos -> Pos -> [Pos] -> [Pos]
    check prev cur acc = case pmap !? cur of
            Nothing -> acc
            Just curVal | succ (pmap ! prev) /= curVal -> acc
                        | curVal == '9' -> f cur $ if cur `elem` acc then acc
                                                   else cur : acc
                        | otherwise -> f cur acc

p2 :: Map Pos Char -> Int
p2 pmap = M.foldlWithKey' start 0 pmap
    where
    f path cur acc = foldl' (flip (recurse path cur)) acc $ adjacent cur

    start acc pos '0' = acc + length (f [] pos [])
    start acc _ _ = acc

    recurse path prev cur acc = case pmap !? cur of
            Nothing -> acc
            Just curVal | succ (pmap ! prev) /= curVal -> acc
                        | curVal == '9' ->
                            let path' = cur : path
                             in f path' cur $ if path' `elem` acc then acc
                                              else path' : acc
                        | otherwise -> f (cur : path) cur acc

distinct paths = 
    let intersects = M.filter (>1) $ M.fromListWith (+) $ (,1) <$> concat paths
     in length $ filter (any (`M.notMember` intersects)) paths
