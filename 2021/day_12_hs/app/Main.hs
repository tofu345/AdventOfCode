module Main where

import qualified Data.Map as Map
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let caves = parse contents
    -- putStrLn contents
    -- print caves

    partOne caves
    partTwo caves

type Caves = Map.Map String [String]

parse :: String -> Caves
parse contents =
    foldl
        (\acc v ->
            let [x, y] = splitOn "-" v
            in insertNotStart y x (insertNotStart x y acc))
        Map.empty
        $ lines contents

    where insertNotStart k v map =
            if v == "start"
                then map
                else Map.insertWith (++) k [v] map

type Path = [String]

partOne :: Caves -> IO ()
partOne caves = do
    let res = recurse ["start"] [] []
    putStr "Part One: "
    print (length res)

    where recurse :: [String] -> Path -> [Path] -> [Path]
          recurse [] _ acc = acc
          recurse (curr:rest) path acc
            | curr == "end" = recurse rest path $ ("end" : path) : acc
            | all isLower curr && curr `elem` path = recurse rest path acc
            | otherwise = let acc' = recurse rest path acc
                              connections = fromJust $ Map.lookup curr caves
                           in recurse connections (curr : path) acc'

-- im lost... not anymore :>
partTwo :: Caves -> IO ()
partTwo caves = do
    let res = recurse ["start"] [] []
    putStr "Part Two: "
    print (length res)

    where recurse :: [String] -> Path -> [Path] -> [Path]
          recurse [] _ acc = acc
          recurse (curr:rest) path acc
            | curr == "end" = recurse rest path $ ("end" : path) : acc
            | all isLower curr && curr `elem` path && 
                (any (\v -> length v >= 2) . group . sort . filter (all isLower) 
                    $ path)
                   = recurse rest path acc
            | otherwise = 
                let acc' = recurse rest path acc
                    connections = fromJust $ Map.lookup curr caves
                 in recurse connections (curr : path) acc'

