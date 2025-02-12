module Main (main) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let plot = M.fromList [ ((x, y), v) | (y, line) <- zip [0..] (lines contents)
                                        , (x, v) <- zip [0..] line ]
        groups = group' plot

    putStr "Part One: "
    print $ price groups

    putStr "Part Two: "
    print $ discountPrice groups

type Pos = (Int, Int)
type Region = Set Pos
type GardenPlot = Map Pos Char

adjacent :: Pos -> [Pos]
adjacent (x, y) = [ (x, y - 1)
                  , (x + 1, y)
                  , (x, y + 1)
                  , (x - 1, y)
                  ]

edges :: Pos -> [[Pos]]
edges p@(x, y) = zipWith (:) res $ pairs (adjacent p)
    where
    pairs [a, b, c, d] = [[a, b], [b, c], [c, d], [d, a]]
    pairs _ = error "unreachable"
    res = [ (x + 1, y - 1)
          , (x + 1, y + 1)
          , (x - 1, y + 1)
          , (x - 1, y - 1)
          ]

group' :: GardenPlot -> [Set Pos]
group' plot = M.foldlWithKey' f [] plot
    where
    f acc pos val
        | any (S.member pos) acc = acc
        | otherwise = dfs pos val (S.singleton pos) : acc
    dfs pos val reg = foldl' g reg $ adjacent pos
        where
        g reg' pos'
            | S.member pos' reg' = reg'
            | Just val' <- M.lookup pos' plot =
                if val' == val then dfs pos' val' (S.insert pos' reg')
                else reg'
            | otherwise = reg'

count :: (a -> Bool) -> [a] -> Int
count f xs = length (filter f xs)

price :: [Region] -> Int
price = sum . map solve
    where
    solve region = let fences = count (`S.notMember` region) . adjacent
                    in S.size region * sum (map fences $ S.toList region)

discountPrice :: [Region] -> Int
discountPrice = sum . map solve
    where
    solve region =
        let isCorner ps = case map (`S.notMember` region) ps of
                [_, True, True] -> True
                [True, False, False] -> True
                _ -> False
            corners pos = count isCorner (edges pos)
         in S.size region * sum (map corners $ S.toList region)
