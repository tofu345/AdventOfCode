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
          recurse ("end":rest) path acc =
              recurse rest path $ ("end" : path) : acc
          recurse (curr:rest) path acc =
                if all isLower curr && curr `elem` path
                    then recurse rest path acc
                    else
                        let acc' = recurse rest path acc
                            connected = fromJust $ Map.lookup curr caves
                        in recurse connected (curr : path) acc'

-- im lost... not anymore :>
partTwo :: Caves -> IO ()
partTwo caves = do
    let res = recurse ["start"] [] []
    putStr "Part Two: "
    print (length res)

    where recurse :: [String] -> Path -> [Path] -> [Path]
          recurse [] _ acc = acc
          recurse ("end":rest) path acc =
              recurse rest path $ ("end" : path) : acc
          recurse (curr:rest) path acc =
                if all isLower curr && curr `elem` path
                   then
                       let smallCaves =
                                group . sort . filter (all isLower)
                                $ path
                       in if any (\v -> length v >= 2) smallCaves
                           then recurse rest path acc
                           else continue
                    else continue
            where continue =
                    let acc' = recurse rest path acc
                        connected = fromJust $ Map.lookup curr caves
                    in recurse connected (curr : path) acc'

