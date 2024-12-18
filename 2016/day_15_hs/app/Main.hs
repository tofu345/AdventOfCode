module Main where

testData =
    [ (5, 4)
    , (2, 1)
    ]

inputData =
    [ (7, 0)
    , (13, 0)
    , (3, 2)
    , (5, 2)
    , (17, 0)
    , (19, 7)
    ]

main :: IO ()
main = do
    putStr "Part One: "
    print $ findTime inputData
    putStr "Part Two: "
    print $ findTime $ inputData ++ [(11, 0)]
    where
    findTime :: [(Int, Int)] -> Int
    findTime positions = until valid (+1) 0
        where valid t = all (==0) [ (i + t + s) `mod` n | (i, (n, s)) <- zip [1..] positions ]
