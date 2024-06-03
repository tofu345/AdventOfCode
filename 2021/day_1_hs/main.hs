import System.IO

main = do
    contents <- readFile "input.txt"
    let contents' = map read (lines contents)
    let ansOne = partOne contents'
    putStrLn ("Part One: " ++ show ansOne)
    let ansTwo = partOne $ partTwo contents' []
    putStrLn ("Part Two: " ++ show ansTwo)

partOne :: [Int] -> Int
partOne [] = 0
partOne [x] = 0
partOne (prev:curr:list) =
    (if curr > prev
         then 1
         else 0)
        + partOne (curr : list)

partTwo :: [Int] -> [Int] -> [Int]
partTwo [] _ = []
partTwo (curr:list) prevArr =
    let newPrev = curr : prevArr
     in if length prevArr == 2
            then sum newPrev : partTwo list (init newPrev)
            else partTwo list newPrev
