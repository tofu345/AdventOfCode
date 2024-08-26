module Main where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import Data.Semigroup
import Data.Maybe (fromJust)

import qualified Dijsktras as D

main = do
    let file =  "input.txt"
    contents <- lines <$> readFile file
    let cons = parse contents
        dests = M.keys cons
        res = foldl
            (\acc c -> partOne cons c 0 dests <> acc)
            Nothing
            $ M.keys cons

    putStr "Part One: "
    print $ getMin $ fromJust res

    let res' = foldl
            (\acc c -> partTwo cons c 0 dests <> acc)
            Nothing
            $ M.keys cons
    putStr "Part Two: "
    print $ getMax $ fromJust res'

    putStr "Dijsktras: "
    D.main cons

type Conns = Map String [(String, Int)]

parse :: [String] -> Conns
parse lines' = M.fromListWith (++) $ do
    [dir, dist] <- map (splitOn " = ") lines'
    let [a, b] = splitOn " to " dir
    [(a, [(b, read dist)]), (b, [(a, read dist)])]

partOne :: Conns -> String -> Int -> [String] -> Maybe (Min Int)
partOne cons curr dist notVisited = do
    guard (curr `elem` notVisited)
    if length notVisited == 1
        then return (Min dist)
        else M.lookup curr cons >>= foldl f Nothing
    where
    f acc (dest, nextDist) = acc <>
        partOne cons dest (dist+nextDist) (filter (curr /=) notVisited)

partTwo :: Conns -> String -> Int -> [String] -> Maybe (Max Int)
partTwo cons curr dist notVisited = do
    guard (curr `elem` notVisited)
    if length notVisited == 1
        then return (Max dist)
        else M.lookup curr cons >>= foldl f Nothing
    where
    f acc (dest, nextDist) = acc <>
        partTwo cons dest (dist+nextDist) (filter (curr /=) notVisited)
