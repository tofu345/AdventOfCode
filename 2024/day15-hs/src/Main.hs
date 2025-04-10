module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (find)
import Data.Foldable (foldl')
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Control.Monad

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let (warehouse, xs) = break null contents
        moves = concat $ drop 1 xs
        hmap = M.fromList [ ((x, y), v) | (y, line) <- zip [0..] warehouse
                                        , (x, v) <- zip [0..] line
                                        , v /= '.' ] :: Map Pos Char
        start = case find ((== '@') . snd) (M.toList hmap) of
                Just (p, _) -> p
                Nothing -> error "start position not found"
        (_, hmap1) = foldl' partOne (start, hmap) moves

    putStr "Part One: "
    print (score hmap1)

    let doubleX (x, y) = (x * 2, y)
        start' = doubleX start
        hmap' = M.mapKeysMonotonic doubleX hmap
        (_, hmap2) = foldl' partTwo (start', hmap') moves

    putStr "Part Two: "
    print (score hmap2)

    -- display hmap
    -- display2 hmap2
    where
    score = sum . map (\(x, y) -> 100 * y + x) . M.keys . M.filter (=='O')

type Pos = (Int, Int)
type Dir = Char

-- display :: Map Pos Char -> IO ()
-- display hmap = do
--     let ((xMax, yMax), _) = M.findMax hmap
--     forM_ [0..yMax] $ \y -> do
--         forM_ [0..xMax] $ \x -> do
--             case M.lookup (x, y) hmap of
--                 Just v -> putChar v
--                 Nothing -> putChar ' '
--         putStrLn ""
--
-- display2 :: Map Pos Char -> IO ()
-- display2 hmap = do
--     let ((xMax, yMax), _) = M.findMax hmap
--     forM_ [0..yMax] $ \y -> do
--         forM_ [0..xMax] $ \x -> do
--             case M.lookup (x, y) hmap of
--                 Just 'O' -> putChar '['
--                 Just '#' -> putChar '#'
--                 Just '@' -> putChar '@'
--                 Nothing ->
--                     let prev = M.lookup (x - 1, y) hmap
--                      in case prev of
--                          Just '#' -> putChar '#'
--                          Just 'O' -> putChar ']'
--                          _ -> putChar ' '
--         putStrLn ""

posIn :: Dir -> Pos -> Pos
posIn '<' (x, y) = (x - 1, y)
posIn '>' (x, y) = (x + 1, y)
posIn 'v' (x, y) = (x, y + 1)
posIn '^' (x, y) = (x, y - 1)
posIn _ _ = error "invalid direction"

partOne :: (Pos, Map Pos Char) -> Dir -> (Pos, Map Pos Char)
partOne (cur, hmap) dir =
    let (ch, pos) = furthestNonBox
     in case ch of
        '#' -> (cur, hmap)
        '.' -> let next = posIn dir cur
                in if pos == next then (next, moveCurTo next hmap)
                   else (next, M.insert pos 'O' . moveCurTo next $ hmap)
        _ -> error "unreachable"
    where
    moveCurTo next = M.insert next '@' . M.delete cur
    furthestNonBox = f cur
        where
        f p = let p' = posIn dir p
               in case M.lookup p' hmap of
                   Just 'O' -> f p'
                   Just _ -> ('#', p')
                   Nothing -> ('.', p')

partTwo :: (Pos, Map Pos Char) -> Dir -> (Pos, Map Pos Char)
partTwo (cur, hmap) dir =
     case moveableBoxesIn dir cur hmap of
        Nothing -> (cur, hmap) -- wall
        Just boxes ->
            let next = posIn dir cur
             in (next, moveCurTo next . moveBoxes boxes $ hmap)
    where
    moveCurTo next = M.insert next '@' . M.delete cur
    moveBoxes boxes hmap' =
        let movedBoxes = S.fromList $ posIn dir <$> boxes
            hmap'' = foldl' f hmap' movedBoxes
         in foldl' g hmap'' $ S.difference (S.fromList boxes) movedBoxes
    f hmap' p = M.insert p 'O' hmap'
    g hmap' p = M.delete p hmap'

-- Good luck future me understanding how this works
moveableBoxesIn :: Dir -> Pos -> Map Pos Char -> Maybe [Pos]
moveableBoxesIn dir pos hmap
    | dir `elem` ['<', '>'] = leftright pos []
    | otherwise = updown [pos] []
    where
    valAt = fromMaybe '.' . (hmap M.!?)
    leftright cur path = case next of
        ('.', _) -> Just path
        ('O', p) -> let p' = if dir == '>' then posIn dir p else p
                     in leftright p' (p : path)
        _ -> Nothing
        where
        next = let p = posIn dir cur
                   v = valAt p
                in case () of _ | dir == '>' -> (v, p)
                                | v == '.' -> let p' = posIn dir p in (valAt p', p')
                                | otherwise -> (v, p)
    updown [] path = Just path
    updown (cur:xs) path =
        let p = posIn dir cur
         in case valAt p of
            'O' -> let p' = first (+1) p
                    in p' `seq` updown (p : p' : xs) (p:path)

            '.' -> let p' = first (subtract 1) p
                    in case valAt p' of 'O' -> updown (p':p:xs) (p':path)
                                        '#' -> Nothing
                                        _ -> updown xs path
            _ -> Nothing
