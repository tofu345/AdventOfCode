module Main where

import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), (<|), (|>), (><))
import Data.Char (digitToInt)
import Data.Foldable (foldl', foldr')
import Data.Maybe (listToMaybe, isJust)

-- TODO: Goals https://aoc.csokavar.hu/2024/9/

-- main :: IO ()
main = do
    --          v: drop \n
    contents <- init <$> readFile "input.txt"
    when (even $ length contents) (error "invalid data")
    let diskMap = parse $ map digitToInt contents

    putStr "Part One: "
    print $ p1 diskMap

    -- 6381528943428, too low
    putStr "Part Two: "
    print $ p2 diskMap

    return diskMap

type Size = Int
data Block = Block { idNum :: Int, len :: Size, after :: Size } deriving (Show)

parse :: [Int] -> Seq Block
parse = p 0 S.empty
    where
    p id ys (fl:fr:xs) = p (id + 1) (ys |> Block id fl fr) xs
    p id ys [fl] = ys |> Block id fl 0
    p _ ys [] = ys

p1 :: Seq Block -> Int
p1 = f 0 0
    where
    f acc idx (cur :<| blks) =
        let acc' = foldl' (\acc i -> acc + (idx + i) * idNum cur) acc [0..len cur - 1]
            idx' = idx + len cur
         in if after cur == 0 then f acc' idx' blks
            else case blks of
                Empty -> acc'
                (blks' :|> end)
                    | len end == after cur -> f acc' idx' $ end { after = 0 } <| blks'
                    | len end > after cur ->
                         f acc' idx' $ (end { len = after cur, after = 0 } <| blks')
                                     |> end { len = len end - after cur }
                    | otherwise ->
                        f acc' idx' $ end { after = after cur - len end } <| blks'

p2 :: Seq Block -> Int
p2 bs = checksum 0 0 $ foldr' defragment bs bs
    where
    checksum acc _ Empty = acc
    checksum acc idx (cur :<| blks) = do
        let acc' = foldl' (\a i -> a + (idx + i) * idNum cur) acc [0..len cur - 1]
         in checksum acc' (idx + len cur + after cur) blks

defragment _ Empty = Empty
defragment cur (b :<| bs)
    | idNum b == idNum cur && len b == len cur = b <| bs
    | after b >= len cur = b { after = 0 }
                        <| cur { after = after b - len cur }
                        <| remove cur bs
    | otherwise = b <| defragment cur bs

remove b blks =
    let (l, b' :<| r) = S.breakl (\c -> idNum b == idNum c) blks
     in case l of
             (blks' :|> e) ->
                (blks' |> e { after = after e + len b' + after b' }) >< r
             Empty -> r
