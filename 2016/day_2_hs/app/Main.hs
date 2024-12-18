module Main where

import Control.Monad
import Data.Maybe

main = do
    contents <- lines <$> readFile "input.txt"

    putStrLn $ "Part One: " ++ findCode keypad1 (1, 1) contents
    putStrLn $ "Part Two: " ++ findCode keypad2 (2, 0) contents

keypad1 =
    [ ['1', '2', '3']
    , ['4', '5', '6']
    , ['7', '8', '9']
    ]

keypad2 =
    [ ['\0', '\0', '1']
    , ['\0', '2', '3', '4']
    , ['5', '6', '7', '8', '9']
    , ['\0', 'A', 'B', 'C']
    , ['\0', '\0', 'D']
    ]

lookup' k (y, x) = k !! y !! x

move (y, x) 'U' = (y - 1, x)
move (y, x) 'R' = (y, x + 1)
move (y, x) 'D' = (y + 1, x)
move (y, x) 'L' = (y, x - 1)

type Keypad = [String]
type Start = (Int, Int)

findCode :: Keypad -> Start -> [String] -> String
findCode _ curr [] = []
findCode keypad curr (l:rest) = do
    let next = foldl f curr l
    lookup' keypad next : findCode keypad next rest

    where
    f :: (Int, Int) -> Char -> (Int, Int)
    f acc v = fromMaybe acc (move' acc v)

    move' v d = do
        let (y1, x1) = move v d
        guard (y1 >= 0 && x1 >= 0 && y1 <= length keypad - 1)
        let row = keypad !! y1
        guard (x1 < length row)
        let col = row !! x1
        if col == '\0'
            then Nothing
            else Just (y1, x1)
