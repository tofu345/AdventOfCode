module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Regex.Posix
import Control.Monad (foldM)
import Data.Maybe
import Data.List
import Data.Ord
import GHC.IO (mkUserError)

main :: IO ()
main = do
    contents <- readFile "test.txt"
    let (polymer, pairs) = parse contents
    -- partOne pairs start 10
    partOne pairs polymer 20

type Steps = Int
type Pairs = Map.Map String Char

parse :: String -> (String, Pairs)
parse contents = do
    let start = head (lines contents)
    let pairs = foldl
            (\acc v ->
                let [[_, from, to]] = v =~ "(.*) -> (.*)" :: [[String]]
                 in Map.insert from (head to) acc)
            Map.empty
            $ drop 2 (lines contents)
    (start, pairs)

partOne :: Pairs -> String -> Steps -> IO ()
partOne pairs polymer stepsLeft = do
    print stepsLeft
    if stepsLeft > 0
        then partOne pairs (head polymer : recurse polymer) (stepsLeft - 1)
        else do
            -- let quantities = sortBy (comparing Down) 
            --         . Map.elems $ foldl
            --             (\acc v -> Map.insertWith (+) v 1 acc)
            --             Map.empty
            --             polymer

            print $ Map.toList (Map.fromListWith (+) [(x, 1) | x <- polymer]) 

            -- print (length polymer)
            -- print $ head quantities - last quantities
            -- A hashmap might be better
            -- let quantities' = sortBy (comparing Down) . map length . group . sort $ polymer
            -- print $ head quantities' - last quantities'
    where
        recurse :: String -> String
        recurse [] = []
        recurse [_] = []
        recurse (a:b:rest) =
            let c = fromJust $ Map.lookup (a : [b]) pairs
             in c : b : recurse (b : rest)

-- partTwo ::
