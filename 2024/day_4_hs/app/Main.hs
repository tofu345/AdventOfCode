module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

type Point = (Int, Int)
type HashMap = Map Point Char

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    -- great stuff: https://www.reddit.com/r/adventofcode/comments/zkc974/python_data_structures_for_2d_grids/
    let hmap = M.fromList [ ((x, y), val) | (y, line) <- zip [0..] contents,
                                            (x, val) <- zip [0..] line ]
    run hmap (length (head contents), length contents)

run :: HashMap -> Point -> IO ()
run hmap (xMax, yMax) = do
    let start = (0, 0)
        keys' = M.keys hmap

    putStr "Part One: "
    print $ foldl' p1 0 keys'

    putStr "Part Two: "
    print $ foldl' p2 0 keys'
    where
    p1 :: Int -> Point -> Int
    p1 acc p | M.lookup p hmap == Just 'X' = 
                 acc + length (filter (== "XMAS") (adjacent p))
             | otherwise = acc

    adjacent p = do
        (x', y') <- [ (-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1) ]
        let f (x, y) = (x + x', y + y')
            diag = take 4 $ iterate f p
        guard (all (`M.member` hmap) diag)
        [(M.!) hmap <$> diag]

    p2 :: Int -> Point -> Int
    p2 acc p 
        | M.lookup p hmap == Just 'A' = 
            let (m1, m2) = splitAt 3 (adjX p)
                mas = ["MAS", "SAM"]
             in if m1 `elem` mas && m2 `elem` mas
                    then acc + 1
                    else acc
        | otherwise = acc

    adjX :: Point -> [Char]
    adjX (x, y) = fromMaybe []
                $ mapM (`M.lookup` hmap)
                [(x - 1, y - 1), (x, y), (x + 1, y + 1), (x + 1, y - 1), (x, y), (x - 1, y + 1)]
