module Main where

main = do
    contents <- lines <$> readFile "input.txt"
    let data' = map (map (:[])) contents

    putStr "Part One: "
    print $ sum (map partOne data')

    putStr "Part Two: "
    print $ sum (map partTwo data')

partOne :: [String] -> Int
partOne [] = 0
partOne (['\"']:xs) = 1 + partOne xs
partOne (['\\']:"x":xs) = 3 + partOne xs
partOne (['\\']:['\\']:xs) = 1 + partOne xs
partOne (_:xs) = partOne xs

partTwo :: [String] -> Int
partTwo [] = 0
partTwo (['\"']:xs) = 2 + partTwo xs
partTwo (['\\']:"x":xs) = 1 + partTwo xs
partTwo (['\\']:['\\']:xs) = 2 + partTwo xs
partTwo (_:xs) = partTwo xs
