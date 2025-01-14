module Main where

import Text.Regex.Posix

type Data = (Int, Int, Char, String)

main :: IO ()
main = do
    data' <- map parse . lines <$> readFile "input.txt"
    print $ length $ filter p1 data'
    print $ length $ filter p2 data'
    return ()
    where
    parse :: String -> Data
    parse str = 
        let [[_, mn, mx, ch, str']] = str =~ "([0-9]*)-([0-9]*) ([a-z]): ([a-z]*)" :: [[String]]
         in (read mn, read mx, head ch, str')

countElem :: (Eq a) => a -> [a] -> Int
countElem t xs = length $ filter (== t) xs

p1 :: Data -> Bool
p1 (mn, mx, ch, str) = 
    let count = countElem ch str
     in count >= mn && count <= mx

p2 :: Data -> Bool
p2 (mn, mx, ch, str) = 
    let a = str !! (mn - 1)
        b = str !! (mx - 1)
     in (a == ch) /= (b == ch)
