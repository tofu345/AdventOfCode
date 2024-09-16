{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.DList as D
import Data.Foldable (foldl')
import Data.STRef
import Control.Monad.ST
import Text.Regex.Posix

type Steps = Int
type Pairs = Map String Char
type Count = Map Char Int
type Memo = Map (String, Steps) Count

data Tree a
    = Empty
    | Node { value :: a
           , stepsLeft :: Steps
           , parent :: Tree a }
    deriving (Show)

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
    mapM_ (f countRef memoRef) (group' xs)
    readSTRef countRef
    where
    f :: STRef s Count -> STRef s Memo -> String -> ST s ()
    f m mem value = grow' m mem Node
        { value, stepsLeft = numSteps, parent = Empty}

    group' :: String -> [String]
    group' (a:b:xs) = [a, b] : group' (b : xs)
    group' _ = []

    grow' :: STRef s Count -> STRef s Memo -> Tree String -> ST s ()
    grow' _ _ cur@Node {stepsLeft = 0} = return ()
    grow' countRef memoRef cur@Node {value = val@[a, b], stepsLeft} = do
        oldCount <- readSTRef countRef
        mem <- readSTRef memoRef
        case M.lookup (val, stepsLeft) mem of
            Nothing -> do
                let ch = pairs M.! val
                mapM_ (\v -> grow' countRef memoRef Node
                        { value = v, stepsLeft = stepsLeft - 1
                        , parent = cur })
                    [[a, ch], [ch, b]]
                modifySTRef' countRef (M.insertWith (+) ch 1)
                newCount <- readSTRef countRef
                modifySTRef' memoRef 
                    (M.insert (val, stepsLeft) (oldCount `diff` newCount))
            Just v -> modifySTRef' countRef (`add` v)

add :: Count -> Count -> Count
add m1 m2 = foldl f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (+) k v acc

diff :: Count -> Count -> Count
diff m1 m2 = foldl f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (-) k v acc

countChars :: String -> Map Char Int
countChars = foldl' f M.empty
    where f acc v = M.insertWith (+) v 1 acc
