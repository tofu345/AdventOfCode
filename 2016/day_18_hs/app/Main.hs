module Main where

import Data.Array

testData = ".^^.^.^^^^"
inputData = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."

-- not much laziness

main :: IO ()
main = do
    let inputData' = listArray (0, length inputData - 1) inputData
        p1 = run inputData' 40
    putStr "Part One: "
    print $ length $ concatMap (filter (=='.') . elems) p1

    let p2 = run inputData' 400000
    putStr "Part One: "
    print $ length $ concatMap (filter (=='.') . elems) p2
    where
    run :: Row -> Int -> [Row]
    run prev stepsLeft
        | stepsLeft == 0 = []
        | otherwise =
            let prevLen = snd (bounds prev)
                currRow = findCurrWith prev <$> [0..prevLen]
             in prev : run (listArray (0, prevLen) currRow) (stepsLeft - 1)

type Row = Array Int Char

findCurrWith :: Row -> Int -> Char
findCurrWith prevRow idx =
    let prev = lookup' prevRow <$> [idx - 1, idx, idx + 1]
     in if prev `notElem` ["^^.", ".^^", "^..", "..^"] then '.' else '^'
    where
    lookup' arr idx | idx < 0 || idx > snd (bounds arr) = '.'
                    | otherwise = arr ! idx
