{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Monad

main = do
    contents <- B.lines <$> B.readFile "input.txt"
    let ingredients = case mapM (parseOnly parser) contents of
            Left _ -> error "invalid data"
            Right v -> v
        comb = combinations' (length ingredients)
        scores = map (score ingredients) comb

    putStr "Part One: "
    print $ maximum scores

    let scores2 = map (score ingredients) $ filter
            ((==500) . sum . zipWith (*) (map calories ingredients)) comb

    putStr "Part Two: "
    print $ maximum scores2

data Ingredient = Ingredient
    { name :: B.ByteString
    , capacity :: Int
    , durability :: Int
    , flavor :: Int
    , texture :: Int
    , calories :: Int
    } deriving (Show)

parser = do
    name <- takeTill (==':')
    stringCI ": capacity "
    capacity <- signed decimal
    stringCI ", durability "
    durability <- signed decimal
    stringCI ", flavor "
    flavor <- signed decimal
    stringCI ", texture "
    texture <- signed decimal
    stringCI ", calories "
    calories <- signed decimal
    return Ingredient
        { name, capacity, durability
        , flavor, texture, calories }

score ingredients amounts =
    let cap = sum $ zipWith (*) amounts $ map capacity ingredients
        dur = sum $ zipWith (*) amounts $ map durability ingredients
        flav = sum $ zipWith (*) amounts $ map flavor ingredients
        tex = sum $ zipWith (*) amounts $ map texture ingredients
     in max 0 cap * max 0 dur * max 0 flav * max 0 tex

combinations' :: Int -> [[Int]]
combinations' n = f (n - 1) $ map (:[]) [1..99]
    where
    f :: Int -> [[Int]] -> [[Int]]
    f 0 xs = xs
    f n xs = f (n - 1) $ do
        x <- xs
        y <- [1..99]
        when (n == 1) $ guard (sum x + y == 100)
        return (y : x)
