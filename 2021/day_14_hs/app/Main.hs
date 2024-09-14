module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Text.Regex.Posix
import qualified Data.DList as D
import Control.Monad.ST
import Data.STRef
import Control.Monad

main = do
    contents <- lines <$> readFile "input.txt"
    let pairs = foldl parse M.empty (drop 2 contents)
        p1 = growStr 10 (head contents) pairs
        p1Count = countChars p1
        -- ST Monad practice :>
        p2 = M.elems $ runST $ do
            let polymer = growStr 10 p1 pairs
            prev <- newSTRef M.empty
            quant <- newSTRef $ M.fromListWith (+)
                [(x, 1) | x <- polymer]
            partTwo 20 polymer pairs quant prev
            readSTRef quant

    putStr "Part One: "
    print $ maximum p1Count - minimum p1Count

    putStrLn "runtime: ~23s"
    putStr "Part Two: "
    print $ maximum p2 - minimum p2
    where
    parse :: Pairs -> String -> Pairs
    parse acc v =
        let [[_, k, val]] = v =~ "(.*) -> (.*)" :: [[String]]
         in M.insert k (head val) acc

    growStr :: Steps -> String -> Pairs -> String
    growStr steps str pairs =
        iterate (\s -> head s : fn s) str !! steps
        where
        fn (x:y:rest) =
            let z = pairs M.! [x, y]
             in z : y : fn (y : rest)
        fn _ = []

type Steps = Int
type Pairs = Map String Char
type Quantities = Map Char Int
type Memo = Map String Quantities

add :: Quantities -> Quantities -> Quantities
add m1 m2 = foldl f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (+) k v acc

difference' :: Quantities -> Quantities -> Quantities
difference' m1 m2 = foldl f m1 $ M.toList m2
    where f acc (k, v) = M.insertWith (-) k v acc

partTwo :: Steps -> String -> Pairs -> STRef s Quantities -> STRef s Memo -> ST s ()
partTwo steps (a:b:xs) pairs q p = do
    prev <- readSTRef p
    let str = [a, b]
        ch = pairs M.! str
    case M.lookup str prev of
        Nothing -> do
            m <- newSTRef M.empty
            m' <- grow m str steps
            modifySTRef' q (`add` m')

            prev' <- readSTRef p
            when (M.notMember str prev')
                $ modifySTRef' p (M.insert str m')
        Just v -> do
            modifySTRef' q (`add` v)

    partTwo steps (b : xs) pairs q p

    where
    f acc v = M.insertWith (+) v 1 acc

    grow :: STRef s (Map Char Int) -> String -> Int -> ST s (Map Char Int)
    grow m _ 0 = readSTRef m
    grow m s steps = do
        s' <- growStr' m s $ D.fromList [head s]
        grow m (D.toList s') (steps - 1)

    growStr' :: STRef s (Map Char Int) -> String -> D.DList Char -> ST s (D.DList Char)
    growStr' m (x:y:rest) prev = do
        let z = pairs M.! [x, y]
        modifySTRef' m (M.insertWith (+) z 1)
        growStr' m (y : rest) (prev `D.snoc` z `D.snoc` y)
    growStr' _ _ prev = return prev

partTwo _ _ _ _ _ = return ()

countChars :: String -> Map Char Int
countChars = foldl f M.empty
    where f acc v = M.insertWith (+) v 1 acc
