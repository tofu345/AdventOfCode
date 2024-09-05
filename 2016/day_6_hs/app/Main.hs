{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Data.List
import Data.Function

main :: IO ()
main = do
    messages <- lines <$> readFile "input.txt"
    let maps = M.fromList $ map (, M.empty) [0..length $ head messages]
        chars = filter (not . null)
            $ map M.toList (M.elems $ foldl' f maps messages)

    putStrLn $ "Part One: " ++ map (fst . maximumBy (compare `on` snd)) chars
    putStrLn $ "Part Two: " ++ map (fst . minimumBy (compare `on` snd)) chars
    where
    f acc str = foldl' f' acc $ zip [0..] str
    f' acc (i, ch) = M.update (upd ch) i acc
    upd ch m = Just $ M.insertWith (+) ch 1 m
