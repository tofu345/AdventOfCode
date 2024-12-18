module Main where

import Data.List (nub)
import Data.Array
import Data.Bifunctor
import Prelude hiding ((!!))

type Arr = Array Int (Array Int Char)
type Index = (Int, Int)
data Dir = N | NW | W | SW | S | SE | E | NE deriving (Show, Enum, Eq)

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let len = length (head contents)
        f = listArray (0, len - 1)
        arr = listArray (0, length contents - 1)
            $ map f contents
    run arr (snd (bounds $ arr ! 0), snd (bounds arr))

(!!) :: Arr -> Index -> Char
arr !! (x, y) = let row = arr ! y
                 in row ! x

run :: Arr -> (Int, Int) -> IO ()
run arr (xMax, yMax) = do
    let start = (0, 0)
        matches = p1 (Just start) []

    putStr "Part One: "
    print $ length matches

    let matches2 = p2 (Just start) []

    putStr "Part Two: "
    print $ length matches2

    where
    p1 :: Maybe Index -> [(Dir, Index)] -> [(Dir, Index)]
    p1 Nothing ms = ms
    p1 (Just i) ms =
        let ms' = if arr !! i == 'X'
                      then [(fst adj, i) | adj <- adjacent i, search i adj] ++ ms
                      else ms
         in p1 (next i) ms'

    search :: Index -> (Dir, Index) -> Bool
    search cur (d, next)
        | not $ inBounds next = False
        | otherwise =
            let c = arr !! cur
                n = arr !! next
             in case (c, n) of
                    v | v `elem` [('X', 'M'), ('M', 'A')] ->
                        search next (dir' next d)
                    ('A', 'S') -> True
                    _ -> False

    next :: Index -> Maybe Index
    next (x, y) | x + 1 <= xMax = Just (x + 1, y)
                | y + 1 <= yMax = Just (0, y + 1)
                | otherwise = Nothing

    adjacent pos = dir' pos <$> enumFrom N

    dir' :: Index -> Dir -> (Dir, Index)
    dir' (x, y) N = (N, (x, y - 1))
    dir' (x, y) W = (W, (x + 1, y))
    dir' (x, y) S = (S, (x, y + 1))
    dir' (x, y) E = (E, (x - 1, y))
    dir' (x, y) NE = (NE, (x - 1, y - 1))
    dir' (x, y) SW = (SW, (x + 1, y + 1))
    dir' (x, y) NW = (NW, (x + 1, y - 1))
    dir' (x, y) SE = (SE, (x - 1, y + 1))

    inBounds (x, y) = x >= 0 && y >= 0 && x <= xMax && y <= yMax

    p2 :: Maybe Index -> [Index] -> [Index]
    p2 Nothing ms = ms
    p2 (Just i) ms =
        let v = [i | adj <- adjacent2 i, valid (snd <$> adj), isX (fst <$> adj)]
            ms' = if arr !! i == 'A' && even (length v)
                      then nub v ++ ms
                      else ms
         in p2 (next i) ms'

    valid s = s `elem` ["MS", "SM"]

    xmas' = [[NW, SE], [SW, NE]]

    isX dirs = dirs `elem` xmas'

    adjacent2 pos = rep $ filter f $ map (dir' pos <$>) xmas'
        where
        f [(_, i1), (_, i2)] = inBounds i1 && inBounds i2
        rep :: [[(Dir, Index)]] -> [[(Dir, Char)]]
        rep xs = map (second (arr !!)) <$> xs
