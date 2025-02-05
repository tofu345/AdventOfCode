module Main (main) where

import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- Saw this solution and now I am sad, https://github.com/tobeannouncd/aoc-hs/blob/main/solutions/Y2024/D11.hs

main :: IO ()
main = do
    contents <- map read . words <$> readFile "input.txt" :: IO [Int]

    putStr "Part One: "
    print $ blink 25 contents

    putStr "Part Two: "
    print $ blink 75 contents

-- marginally better than `length . show` :(
numDigits :: Int -> Int
numDigits num = floor (logBase 10 $ fromIntegral num :: Double) + 1

split :: Int -> (Int, Int)
split num =
    let mid = numDigits num `div` 2
        left = num `div` (10 ^ mid)
        right = num - left * (10 ^ mid)
     in (left, right)

applyRule :: Int -> [Int]
applyRule n
    | n == 0 = [1]
    | even (numDigits n) = let (l, r) = split n
                            in [l, r]
    | otherwise = [n * 2024]

type Length = Int
type Blinks = Int
type Memo = Map (Int, Blinks) Length

-- Is memoization necessary here?

blink :: Blinks -> [Int] -> Int
blink initBlinks initNums = runST $ do
    memoRef <- newSTRef $! M.empty
    f memoRef initNums initBlinks 0
    where
    f :: STRef s Memo -> [Int] -> Blinks -> Int -> ST s Int
    f _ nums 0 acc = return $! acc + length nums
    f memoRef nums depth acc =
        foldM (\acc' n -> do
            mem <- readSTRef memoRef
            case M.lookup (n, depth) mem of
                Just len -> return (acc' + len)
                Nothing -> do
                    let nums' = applyRule n
                    res <- f memoRef nums' (depth - 1) 0
                    modifySTRef' memoRef (M.insert (n, depth) res)
                    return (acc' + res)
            ) acc nums
