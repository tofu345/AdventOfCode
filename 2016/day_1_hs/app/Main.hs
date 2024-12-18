{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List.Split
import Data.List
import Data.Maybe

main :: IO ()
main = do
    ins <- wordsBy (`elem` "\n, ") <$> readFile "input.txt"
    let start = Position North (0,0)
        Position {point} = foldl f start ins

    putStr "Part One: "
    print $ abs (fst point) + abs (snd point)

    let p1 = findDup [] start ins
    putStr "Part Two: "
    print $ abs (fst p1) + abs (snd p1)

    where
    f Position {dir, point} (t:c) =
        let newDir = turn t dir
         in Position newDir
            (move newDir point (read c))

    findDup :: [(Int, Int)] -> Position -> [String] -> (Int, Int)
    findDup _ _ [] = error "no duplicate"
    findDup seen curr (v:rest) = do
        let next = f curr v
            range' = range (point curr) (point next)
         in findDup (seen ++ range') next rest 
            `fromMaybe` find (`elem` range') seen

range (y1, x1) (y2, x2) =
    let l = do
            y <- if y1 > y2 then [y1, y1 - 1..y2] else [y1..y2]
            x <- if x1 > x2 then [x1, x1 - 1..x2] else [x1..x2]
            return (y, x)
    in if null l then [] else init l

data Direction = North | East | South | West
    deriving (Show, Enum)

data Position = Position
    { dir :: Direction
    , point :: (Int, Int)
    } deriving (Show)

turn 'R' = right
turn 'L' = left

right West = North
right v = succ v

left North = West
left v = pred v

move North (y, x) c = (y - c, x)
move East (y, x) c = (y, x + c)
move South (y, x) c = (y + c, x)
move West (y, x) c = (y, x - c)
