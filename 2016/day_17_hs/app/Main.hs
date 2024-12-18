module Main where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import Data.Char
import Data.Foldable
import Data.Function
import Data.Ord
import Data.Maybe

passcode = "pxxbnzuo"

main :: IO ()
main = do
    let min' = minimumBy (compare `on` length)
        p1 = bfs (safe min') [(0, 0)] []
    putStr "Part One: "
    putStrLn $ maybe "fail" reverse p1

    let max' = maximumBy (compare `on` length)
        p2 = bfs (safe max') [(0, 0)] []
    putStr "Part Two: "
    putStrLn $ maybe "fail" (show . length) p2
    where
    bfs :: ([Path] -> Maybe Path) -> [Pos] -> Path -> Maybe Path
    bfs cmp [] _ = Nothing
    bfs cmp (cur:ps) path
        | cur == (3, 3) = Just path
        | otherwise = foldl' f Nothing (neighours cur path)
        where
        f :: Maybe Path -> (Dir, Pos) -> Maybe Path
        f acc (dir, pos) =
            cmp $ catMaybes [acc, bfs cmp (pos : ps) (dir : path)]

type Dir = Char
type Path = [Dir]
type Pos = (Int, Int)

safe :: ([Path] -> Path) -> [Path] -> Maybe Path
safe _ [] = Nothing
safe cmp xs = case cmp xs of [] -> Nothing
                             v -> Just v

neighours :: Pos -> Path -> [(Dir, Pos)]
neighours (x, y) path =
    let hash = B.take 4 $ (encode . MD5.hash)
               (B.pack $ passcode ++ reverse path)
        ns = [ ('U', (x, y - 1))
             , ('D', (x, y + 1))
             , ('L', (x - 1, y))
             , ('R', (x + 1, y)) ]
    in snd <$> filter (open hash) (zip [0..] ns)
    where
    open :: B.ByteString -> (Int, (Dir, Pos)) -> Bool
    open hash (i, (dir, (x', y'))) =
        x' >= 0 && y' >= 0 && x' <= 3 && y' <= 3
        && (> 97) (ord $ hash `B.index` i)

