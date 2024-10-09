module Main where

import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad
import Data.Foldable

main :: IO ()
main = do
    p1 <- partOne (31, 39)
    putStr "Part One: "
    print (length p1 - 1)

    -- had to get help :< 
    -- https://www.reddit.com/r/adventofcode/comments/5i1q0h/comment/db4tecq/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
    putStr "Part Two: "
    print $ length $ nub $ concat $ take 51 locations

locations = iterate (nub . concatMap neighbours) [(1,1)] :: [[(Int, Int)]]

type Position = (Int, Int)

isOpenSpace (x, y) =
    let sum' = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + 1362
     in x >= 0 && y >= 0 && even (popCount sum')

neighbours (x, y) = filter isOpenSpace [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

type Seen = [Position]

partOne :: Position -> IO [Position]
partOne target = do
    let start = (1, 1)
    (res, _) <- recurse [start] [start]
    return $ fromMaybe (error "part one fail") res
    where
    recurse :: [Position] -> Seen -> IO (Maybe [Position], Seen)
    recurse path@(cur:_) s = foldM f (Nothing, s) (neighbours cur)
        where
        f (acc, seen) pos
            | pos == target = return (Just (pos:path), seen)
            | pos `elem` seen = return (acc, seen)
            | otherwise = do
                -- mark as seen if no path is found
                (path', seen') <- recurse (pos : path) (pos : seen)
                return $ case path' of
                             Nothing -> (acc, seen')
                             Just _ -> (min' acc path', seen)

    min' :: Maybe [Position] -> Maybe [Position] -> Maybe [Position]
    min' (Just p1) (Just p2) | length p1 < length p2 = Just p1
                             | otherwise = Just p2
    min' (Just p1) Nothing = Just p1
    min' Nothing (Just p2) = Just p2
    min' _ _ = Nothing
