{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.List.Split

rows = 6
cols = 50
screen = replicate rows $ replicate cols '.'

main = do
    ans <- foldl solve screen 
        . map words . lines 
        <$> readFile "input.txt"

    putStr "Part One: "
    print $ length $ concatMap (filter (=='#')) ans

    mapM_ putStrLn ans

replaceAt :: [[a]] -> a -> (Int, Int) -> [[a]]
replaceAt m v (x, y) =
    let (preR, r:postR) = splitAt (y `mod` length m) m
        (preC, _:postC) = splitAt (x `mod` length r) r
     in preR ++ [preC ++ v : postC] ++ postR

solve m ["rect", coords] = do
    let [x, y] = splitOn "x" coords
    let points = do
            x' <- [0..read x - 1]
            y' <- [0..read y - 1]
            return (x', y')
    foldl (`replaceAt` '#') m points
solve m ["rotate", "row", 'y':'=':i, "by", offset] = do
    let idx = read i
        row = replicate (read offset) '.' ++ m !! idx
        (pre, post) = splitAt cols row
        (preR, _:postR) = splitAt idx m
    preR ++ (post ++ drop (length post) pre) : postR
solve m ["rotate", "column", 'x':'=':i, "by", offset] = do
    let p = map (read i,) [0..rows - 1]
    foldl f m p
    where
    f acc (x, y) = replaceAt acc (m !! y !! x) (x, y + read offset)
solve m _ = error "invalid data"
