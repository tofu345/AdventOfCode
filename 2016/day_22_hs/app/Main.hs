module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Data.Array

-- find all nodes you can transfer data you want to access into
--  starting from closest to start
-- find closest node to transfer data in start node
-- find path to transfer target data to start node

main = do
    usageList <- map words . lines <$> readFile "input.txt"
    let usages = parse <$> drop 2 usageList
        len = length usages
        width = findWidth 0 usages
        height = len `div` width
        usagesArr = listArray (0, len - 1) usages

    print $ idxOf width (0, 1) usagesArr
    print (width, height)
    -- partOne usages
    partTwo (idxOf height) usagesArr

    return usagesArr
    where
    findWidth :: Int -> [Usage] -> Int
    findWidth n (x:xs)
        | fst (pos x) == 1 = n
        | otherwise = findWidth (n + 1) xs

    -- TODO: does not work
    idxOf :: Int -> Pos -> Usages -> Usage
    idxOf h (x, y) us = us ! (x + h * y)

type Usages = Array Int Usage

type Pos = (Int, Int)
data Usage = Usage
    { pos :: Pos
    , size :: Int
    , used :: Int
    , avail :: Int
    } deriving (Show)

instance Eq Usage where
    a == b = pos a == pos b

parse :: [String] -> Usage
parse [name, s, u, a, _] =
    let [x, y] = splitOn "-y" $ fromJust
               $ stripPrefix "/dev/grid/node-x" name
        size = read (init s)
        used = read (init u)
        avail = read (init a)
     in Usage (read x, read y) size used avail
parse s = error $ "Err parsing: " ++ unwords s

partOne :: [Usage] -> IO ()
partOne usages = do
    putStr "Part One: "
    print $ foldl' viablePairs 0 usages
    where
    viablePairs :: Int -> Usage -> Int
    viablePairs acc cur
        | used cur == 0 = acc
        | otherwise =
            acc + length (filter (viable cur) usages)

    viable :: Usage -> Usage -> Bool
    viable a b = a /= b && avail b >= used a

partTwo :: (Pos -> Usages -> Usage) -> Usages -> IO ()
partTwo idxOf usages = do
    -- print $ idxOf (35, 29) usages
    -- print $ dfs ()

    return ()
    where
    adjacent (x, y) = filter inBounds [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    inBounds (x, y) = x >= 0 && x < snd (bounds usages) && y >= 0 && y < snd (bounds usages)
    dfs :: Pos -> Int -> Int -> [Pos] -> [Pos]
    -- dfs cur minSize found = found
    dfs currPos minSize minAvail found = 
        let curr = idxOf currPos usages
         in case () of
             _ | size curr < minSize -> found
               | avail curr >= minAvail -> currPos : found
               | otherwise -> found

