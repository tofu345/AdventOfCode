module Main where

import Data.List.Split

main :: IO ()
main = do
    data' <- map (splitOn " ") . lines <$> readFile "input.txt"
    let len = length data'

    let ans1 = simulate data' (0, 0) len
    putStr "Part One: " >> print (snd ans1)

    let ans2 = simulate data' (1, 0) len
    putStr "Part Two: " >> print (snd ans2)

simulate :: [[String]] -> (Int, Int) -> Int -> (Int, Int)
simulate data' regs len = do
    run regs (head data') 0
    where
    run :: (Int, Int) -> [String] -> Int -> (Int, Int)
    run regs ["hlf", r : _] i = upd regs i (`div` 2) r
    run regs ["tpl", r : _] i = upd regs i (* 3) r
    run regs ["inc", r : _] i = upd regs i (+ 1) r
    run regs ["jmp", offset] i = next regs (calc offset i)
    run regs ["jie", r : _, offset] i =
        if even (regs ! r)
            then next regs (calc offset i)
            else next regs (i + 1)
    run regs ["jio", r : _, offset] i =
        if regs ! r == 1
            then next regs (calc offset i)
            else next regs (i + 1)

    (a, _) ! 'a'  = a
    (_, b) ! 'b'  = b

    next regs i = 
        if i >= len
            then regs
            else run regs (data' !! i) i

    upd :: (Int, Int) -> Int -> (Int -> Int) -> Char -> (Int, Int)
    upd (a, b) i f 'a' = next (f a, b) (i + 1)
    upd (a, b) i f 'b' = next (a, f b) (i + 1)

    calc :: String -> Int -> Int
    calc ('+':xs) i = i + read xs
    calc ('-':xs) i = i - read xs
