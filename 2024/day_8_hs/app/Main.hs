module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Char (isAlphaNum)
import Data.List
import Data.Function (on)

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"

    let hmap = M.fromList [ ((x, y), v) | (y, line) <- zip [0..] contents
                                        , (x, v) <- zip [0..] line ]
        bounds = (length (head contents), length contents)
        -- feels like such shit code
        antennas = M.elems . M.fromListWith (++) . helper . M.toList
                 $ M.filter isAlphaNum hmap

    putStr "Part One: "
    print $ length $ nub $ filter (inBounds bounds) $ p1 antennas

    putStr "Part Two: "
    print $ length $ nub $ p2 bounds antennas

inBounds (xMax, yMax) (x, y) = x >= 0 && x < xMax && y >= 0 && y < yMax

helper :: [(a, b)] -> [(b, [a])]
helper ((a, b):xs) = (b, [a]) : helper xs
helper [] = []

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
