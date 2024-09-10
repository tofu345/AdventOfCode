module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Data.Maybe
import Data.List

target = [17, 61]

main = do
    contents <- map words . lines <$> readFile "input.txt"
    let (m, ins) = parse M.empty [] contents
    putStr "Part One: "
    print $ fromMaybe "fail" $ findTarget m ins

    let m' = solve m ins
    putStr "Part Two: "
    print $ product $ concat [m' ! "output0", m' ! "output1", m' ! "output2"]

type HashMap = Map String [Int]
type Instructions = [(String, String, String)]

parse :: HashMap -> Instructions -> [[String]] -> (HashMap, Instructions)
parse m ins [] = (m, ins)
parse m ins (("value":v:rest):xs) =
    let m' = M.insertWith insert' ("bot" ++ last rest) [read v] m
     in parse m' ins xs
parse m ins ([n, id, "gives", _, _, n1, v1, "and", _, _, n2, v2]:xs) =
    parse m ((n ++ id, n1 ++ v1, n2 ++ v2) : ins) xs
parse m ins (_:xs) = error "invalid data"

insert' [x] [] = [x]
insert' [x] [y] = if x < y then [x, y] else [y, x]

giveChips from [lo, hi] b1 b2 m = 
    M.insertWith insert' b2 [hi]
    $ M.insertWith insert' b1 [lo]
    $ M.delete from m

findTarget :: HashMap -> Instructions -> Maybe String
findTarget m [] = Nothing
findTarget m (curr@(from, bot1, bot2):xs) =
    case M.lookup from m of
        Just v | length v == 2 -> do
            if v == target
                then Just from
                else findTarget (giveChips from v bot1 bot2 m) xs
        _ -> findTarget m (xs ++ [curr])

solve m [] = m
solve m (curr@(from, bot1, bot2):xs) =
    case M.lookup from m of
        Just v | length v == 2 -> solve (giveChips from v bot1 bot2 m) xs
        _ -> solve m (xs ++ [curr])
