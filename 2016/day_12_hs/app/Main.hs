module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Foldable (foldl')
import Data.List

-- I cant bring myself to like this code and it runs in 13 seconds

main :: IO ()
main = do
    dat <- map words . lines <$> readFile "input.txt"
    let dat' = S.fromList dat
    let m = M.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]

    putStr "Part One: "
    print $ solve m 0 dat' M.! "a"

    let m' = M.fromList [("a", 0), ("b", 0), ("c", 1), ("d", 0)]

    putStr "Part Two: "
    print $ solve m' 0 dat' M.! "a"

solve :: Map String Int -> Int -> Seq [String] -> Map String Int
solve m idx dat
    | idx < S.length dat = case dat S.!? idx of
        Nothing -> m
        Just ["cpy", x, y] ->
            let x' = reads' x m
             in solve (M.insert y x' m) (idx + 1) dat
        Just ["inc", r] -> solve (M.update (Just . (+1)) r m) (idx + 1) dat
        Just ["dec", r] -> solve (M.update (Just . subtract 1) r m) (idx + 1) dat
        Just ["jnz", x, y] ->
            let x' = reads' x m
             in case x' of
                0 -> solve m (idx + 1) dat
                _ -> solve m (idx + read y) dat
    | otherwise = m

reads' x regs = case reads x of [] -> regs M.! x
                                [(v, _)] -> v
