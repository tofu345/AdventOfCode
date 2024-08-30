{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Map (foldWithKey)
import qualified Data.Set as S
import qualified Data.List.Split as Split
import Data.List
import Data.Ord
import Data.Maybe
import Data.Function

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let str = take (length contents - 2) contents
        molecule = last contents
        r1 = M.fromListWith (++)
             $ map (\v -> let [a, b] = Split.splitOn " => " v in (a, [b]))
             str
        ans1 = partOne molecule contents r1
    putStr "Part One: "
    print $ length ans1

    let (repsStr, endStr) = break (\v -> head v == 'e') str
        reps = M.fromListWith (++)
             $ map (\v -> let [a, b] = Split.splitOn " => " v in (b, [a]))
             repsStr
        keys = sortBy (flip compare `on` length) (M.keys reps)
        end = mapMaybe (stripPrefix "e => ") endStr
    partTwo molecule reps keys end

replace':: String -> String -> Int -> Int -> String
replace' str sub subLen idx =
    take idx str ++ sub ++ drop (idx + subLen) str

partOne :: String -> [String] -> M.Map String [String] -> S.Set String
partOne molecule contents = M.foldlWithKey f S.empty
    where
    f acc k = foldl (\a v' ->
        let r = map (replace' molecule v' (length k) . snd)
                (indices 0 molecule k)
        in S.fromList r `S.union` a) acc

    indices _ [] _ = []
    indices i str sub  =
        if sub `isPrefixOf` str
            then (sub, i) : indices (i + 1) (tail str) sub
            else indices (i + 1) (tail str) sub

partTwo :: String -> M.Map String [String] -> [String] -> [String] -> IO ()
partTwo molecule reps keys end = do
    putStr "Part Two: "
    print $ recurse [(molecule, 1)]
    where
    recurse :: [(String, Int)] -> Int
    recurse [] = 0
    recurse ((curr, steps):rest)
        | curr `elem` end = steps
        | otherwise = do
            let occurences = mapMaybe (index 0 curr) keys
                molecules = map (, steps + 1) . nub
                    $ concatMap (fn curr) occurences
            recurse (molecules ++ rest)

    fn m (k, i) = map (\v -> replace' m v (length k) i) $ reps M.! k

    index _ [] _ = Nothing
    index i str sub  =
        if sub `isPrefixOf` str
            then Just (sub, i)
            else index (i + 1) (tail str) sub
