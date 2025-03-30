module Main (main) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (find)
import Data.Foldable (foldl')

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let (warehouse, xs) = break null contents
        moves = concat $ drop 1 xs
        hmap = M.fromList [ ((x, y), v) | (y, line) <- zip [0..] warehouse
                                        , (x, v) <- zip [0..] line
                                        , v /= '.' ] :: Map Pos Dir
        start = case find ((== '@') . snd) (M.toList hmap) of
                Just (p, _) -> p
                Nothing -> error "start position not found"
        score = sum . map (\((x, y), _) -> 100 * y + x)

        (_, hmap1) = foldl' simulate (start, hmap) moves
        ans1 = score $ filter ((=='O') . snd) (M.toList hmap1)

    putStr "Part One: "
    print ans1

    -- return (start, hmap)

type Pos = (Int, Int)
type Dir = Char

-- display :: Map Pos Dir -> IO ()
-- display hmap = do
--     let ((xMax, yMax), _) = M.findMax hmap
--     forM_ [0..yMax] $ \y -> do
--         forM_ [0..xMax] $ \x -> do
--             case M.lookup (x, y) hmap of
--                 Just v -> putChar v
--                 Nothing -> putChar ' '
--         putStrLn ""

moveIn :: Dir -> Pos -> Pos
moveIn '<' (x, y) = (x - 1, y)
moveIn '>' (x, y) = (x + 1, y)
moveIn 'v' (x, y) = (x, y + 1)
moveIn '^' (x, y) = (x, y - 1)
moveIn _ _ = error "invalid direction"

furthestNonBox :: Dir -> Pos -> Map Pos Char -> (Char, Pos)
furthestNonBox d p hmap = f p
    where
    f pos = let pos' = moveIn d pos
             in case M.lookup pos' hmap of
                 Just v | v == 'O' -> f pos'
                 Just v -> (v, pos')
                 _ -> ('.', pos')

simulate :: (Pos, Map Pos Char) -> Char -> (Pos, Map Pos Char)
simulate (cur, hmap) dir =
    let (ch, pos) = furthestNonBox dir cur hmap
     in case ch of
        '#' -> (cur, hmap)
        '.' -> let next = moveIn dir cur
                in if pos == next then (next, moveTo next hmap)
                   else (next, M.insert pos 'O' . moveTo next $ hmap)
        _ -> error "unreachable"
    where
    moveTo next = M.insert next '@' . M.delete cur
