module Main where

import Text.Regex.Posix
import Data.Foldable (foldl')

main :: IO ()
main = do
    contents <- readFile "input.txt"

    putStr "Part One: "
    print $ p1 contents

    let syms = contents =~ "(mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\))" :: [[String]]

    putStr "Part Two: "
    print $ p1 $ filter' (head <$> syms)

    where
    p1 :: String -> Int
    p1 s = let nums = s =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [[String]]
            in foldl' sum' 0 nums

    sum' :: Int -> [String] -> Int
    sum' acc [_, l, r] = acc + read l * read r

    takeTill :: String -> [String] -> [String]
    takeTill s [] = []
    takeTill s (c:cs) | s == c = cs
                      | otherwise = takeTill s cs

    filter' :: [String] -> String
    filter' = f []
        where
        f acc ("don't()":xs) = f acc (takeTill "do()" xs)
        f acc ("do()":xs) = f acc xs
        f acc (str:xs) = f (str ++ ' ' : acc) xs
        f acc [] = acc
