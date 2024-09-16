module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Foldable (foldl')
import Data.STRef
import Control.Monad.ST
import Control.Monad
import Text.Regex.Posix

type Steps = Int
type Pairs = Map String Char
type Count = Map Char Steps
type Memo = Map (String, Steps) Count

main = do
    contents <- lines <$> readFile "input.txt"
    let pairs = foldl' parse M.empty (drop 2 contents)
        polymer = head contents
        p1 = grow polymer 10 pairs
        p2 = grow polymer 40 pairs

    putStr "Part One: "
    print $ maximum p1 - minimum p1

    putStr "Part Two: "
    print $ maximum p2 - minimum p2
    where
    parse :: Pairs -> String -> Pairs
    parse acc v =
        let [[_, k, val]] = v =~ "(.*) -> (.*)" :: [[String]]
         in M.insert k (head val) acc

grow :: String -> Steps -> Pairs -> Count
grow [] _ _ = M.empty
grow xs numSteps pairs = runST $ do
    countRef <- newSTRef $! countChars xs
    memoRef <- newSTRef $! M.empty
    mapM_ (\v -> grow' countRef memoRef (v, numSteps)) (group' xs)
    readSTRef countRef
    where
    group' :: String -> [String]
    group' (a:b:xs) = [a, b] : group' (b : xs)
    group' _ = []

    grow' :: STRef s Count -> STRef s Memo -> (String, Steps) -> ST s ()
    grow' _ _ (_, 0) = return ()
    grow' countRef memoRef cur@(value@[a, b], stepsLeft) = do
        mem <- readSTRef memoRef
        case M.lookup cur mem of
            Nothing -> do
                oldCount <- readSTRef countRef
                let ch = pairs M.! value
                mapM_ (\v -> grow' countRef memoRef (v, stepsLeft - 1))
                    [[a, ch], [ch, b]]
                modifySTRef' countRef (M.insertWith (+) ch 1)
                when (stepsLeft < numSteps) $ do
                    newCount <- readSTRef countRef
                    modifySTRef' memoRef 
                        (M.insert cur (diff oldCount newCount))
            Just m -> modifySTRef' countRef (`add` m)

add :: Count -> Count -> Count
add m1 m2 = foldl' f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (+) k v acc

diff :: Count -> Count -> Count
diff m1 m2 = foldl' f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (-) k v acc

countChars :: String -> Map Char Int
countChars = foldl' f M.empty
    where f acc v = M.insertWith (+) v 1 acc
