module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Char (isAlphaNum)
import Data.List

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"

    let bounds = (length (head contents), length contents)
        antennas = M.elems . M.fromListWith (++)
                 $ [ (v, [(x, y)]) | (y, line) <- zip [0..] contents
                                   , (x, v) <- zip [0..] line
                                   , isAlphaNum v ]

    putStr "Part One: "
    print $ length $ nub $ filter (inBounds bounds) $ p1 antennas

    putStr "Part Two: "
    print $ length $ nub $ p2 bounds antennas

inBounds (xMax, yMax) (x, y) = x >= 0 && x < xMax && y >= 0 && y < yMax

type Pos = (Int, Int)

subtractPos :: Pos -> Pos -> Pos
subtractPos (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

p1 :: [[Pos]] -> [Pos]
p1 = foldl' f []
    where
    f acc ns = foldl' g acc $ subsequences ns
    g acc [p1, p2] = h p2 p1 : h p1 p2 : acc
    g acc _ = acc
    h p1 p2 = p2 `addPos` (p2 `subtractPos` p1)

p2 :: Pos -> [[Pos]] -> [Pos]
p2 bounds = foldl' f []
    where
    f acc ns = foldl' g acc $ subsequences ns
    g acc [p1, p2] = h p2 p1 ++ h p1 p2 ++ acc
    g acc _ = acc
    h p1 p2 = let diff = p2 `subtractPos` p1
               in takeWhile (inBounds bounds) $ iterate (addPos diff) p2
