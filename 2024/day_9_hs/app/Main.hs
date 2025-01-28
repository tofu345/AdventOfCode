module Main where

import Control.Monad.ST
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Char (digitToInt)

-- main :: IO ()
main = do
    contents <- init <$> readFile "input.txt"
    let nums = parse $ map digitToInt contents

    putStr "Part One: "
    print $ checksum $ p1 nums

    putStr "Part Two: "
    print $ checksum $ fromBlocks $ p2 $ toBlocks nums

    -- putStr "bef: " >> display nums
    -- putStr "aft: " >> display (fromBlocks $ p2 $ toBlocks nums)

    return nums

-- copied... mostly: https://github.com/dsagman/advent-of-code/blob/main/2024/day9/day9vector.hs
parse :: [Int] -> Vector Int
parse bs = V.concat [V.replicate b (f i) | (i, b) <- zip [0..] bs]
    where f i = if even i then i `div` 2 else -1

p1 :: Vector Int -> Vector Int
p1 orig = runST $ V.thaw orig >>= p1' 0 (V.length orig - 1) >>= V.freeze

p1' :: Int -> Int -> VM.MVector s Int -> ST s (VM.MVector s Int)
p1' i j ns
    | i >= j = return ns
    | otherwise = do
        i' <- VM.read ns i
        j' <- VM.read ns j
        case () of
            _ | i' /= -1 -> p1' (i + 1) j ns
              | j' == -1 -> p1' i (j - 1) ns
              | otherwise -> do
                  VM.write ns i j'
                  VM.write ns j i'
                  p1' (i + 1) (j - 1) ns

checksum :: Vector Int -> Int
checksum = V.ifoldl' f 0
    where f acc idx cur | cur == -1 = acc
                        | otherwise = acc + idx * cur

display :: Vector Int -> IO ()
display ns = V.mapM_ f ns >> putStrLn ""
    where
    f n | n == -1 = putChar '.'
        | otherwise = putStr (show n)

-- inspiration, kinda https://aoc.csokavar.hu/2024/9/

-- really want to keep vector, but vector of ints is not ideal for p2
type ID = Int
type Len = Int
data Block = File ID Len | Free Len deriving Show

toBlocks :: Vector Int -> Vector Block
toBlocks = V.force . V.fromList . f . V.group
    where
    f [] = []
    f (n:ns) | V.head n == -1 = Free (V.length n) : f ns
             | otherwise = File (V.head n) (V.length n) : f ns

fromBlocks :: Vector Block -> Vector Int
fromBlocks = V.concat . map conv . V.toList
    where
    conv (File id len) = V.replicate len id
    conv (Free len) = V.replicate len (-1)

p2 :: Vector Block -> Vector Block
p2 orig = runST $ V.thaw orig >>= p2' 0 (V.length orig - 1) >>= V.freeze

p2' :: Int -> Int -> VM.MVector s Block -> ST s (VM.MVector s Block)
p2' i j bs
    | j <= 0 = return bs
    | i >= j = p2' 0 (j - 1) bs
    | otherwise = do
        i' <- VM.read bs i
        j' <- VM.read bs j
        match i' j'
    where
    match _ (Free _) = p2' i (j - 1) bs
    match (File _ _) _ = p2' (i + 1) j bs
    match (Free l1) j'@(File id l2) = case compare l1 l2 of
        LT -> p2' (i + 1) j bs
        EQ -> do
            VM.swap bs i j
            p2' 0 (j - 1) bs
        GT -> do
            VM.write bs i j'
            VM.write bs j (Free l2)
            bs' <- VM.grow bs 1
            shift' (i + 1) (Free $ l1 - l2) bs'
            p2' 0 j bs'

shift' :: Int -> Block -> VM.MVector s Block -> ST s (VM.MVector s Block)
shift' idx cur bs
    | idx >= VM.length bs = return bs
    | otherwise = do
        next <- VM.read bs idx
        VM.write bs idx cur
        shift' (idx + 1) next bs
