{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Data.List (insertBy, find)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)

type Pos = (Int, Int)

endPos :: Pos
endPos = (70, 70)

numFallen :: Int
numFallen = 1024

positions :: [Pos]
positions = [ (x, y) | x <- [0..fst endPos], y <- [0..snd endPos] ]

data Position
    = Safe
    | Corrupted
    | Traversed -- for [printGrid]
    deriving Show

type Grid = Map Pos Position

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let corrupted = map strToPos contents
        (fallen, rest) = splitAt numFallen corrupted
        grid = foldl
               (\grid' p -> M.insert p Corrupted grid')
               (M.fromList $ (,Safe) <$> positions)
               fallen

        endNode = fromMaybe (error $ "no path after " ++ show numFallen ++ " bytes")
                $ aStar grid
        path = tracePathFrom endNode

    -- printGrid $ foldl (\grid' p -> M.insert p Traversed grid') grid path

    putStr "Part One: "
    print (length path)

    let cutOffByte = partTwo (S.fromList path) rest grid

    putStr "Part Two: "
    print cutOffByte

    where
    strToPos :: String -> Pos
    strToPos str = case splitOn "," str of
        [x, y] -> (read x, read y)
        _ -> error $ "invalid data :" ++ str

data Node = Node
    { parent :: Maybe Node
    , position :: Pos
    , cost :: Double
    } deriving Show

type Queue = [(Double, Pos)]

type Nodes = Map Pos Node

-- | Insert before first node with higher cost.
insert' :: Queue -> Pos -> Double -> Queue
insert' queue pos cost' =
    insertBy (\(c1, _) (c2, _) -> compare c1 c2) (cost', pos) queue

neighbours :: Pos -> [Pos]
neighbours (x, y) =
    [ (x, y - 1)
    , (x + 1, y)
    , (x, y + 1)
    , (x - 1, y)
    ]

-- | returns an Node of endPos if there is a valid path.
aStar :: Grid -> Maybe Node
aStar grid = do
    let start = (0, 0)
        closed = S.empty
        open = [(0.0, start)]
        nodes = M.fromList [(start, Node Nothing start 0.0)]
     in run closed open nodes
    where
    euclideanCost :: Pos -> Double
    euclideanCost (x, y) =
        let sq a = fromIntegral $ a * a
         in sqrt $ sq (x - fst endPos) * sq (y - snd endPos)

    run :: Set Pos -> Queue -> Nodes -> Maybe Node
    run _ [] _ = Nothing
    run closed ((_, pos) : queue) nodes = do
        let adjacent = filter valid $ neighbours pos
            curNode = nodes M.! pos
        case find (== endPos) adjacent of
            Nothing ->
                let (queue', nodes') =
                        foldl' (update curNode) (queue, nodes) adjacent
                    closed' = S.insert pos closed
                 in run closed' queue' nodes'

            _ -> Just $ Node (Just curNode) endPos 0.0
        where
        valid p = case grid M.!? p of
                Just Safe -> S.notMember p closed
                _ -> False

    -- | update node at [newPos] if exists with new cost if new cost is less
    --   than its current cost
    update parentNode (queue, nodes) newPos =
        let newCost = cost parentNode + euclideanCost newPos + 1
         in case nodes M.!? newPos of
            Just (Node _ _ prevCost) | newCost > prevCost -> (queue, nodes)
            _ -> let newNode = Node (Just parentNode) newPos newCost
                     queue' = insert' queue newPos newCost
                     nodes' = M.insert newPos newNode nodes
                  in (queue', nodes')

tracePathFrom :: Node -> [Pos]
tracePathFrom node = case parent node of
    Nothing -> []
    Just parent' -> position node : tracePathFrom parent'

-- | recalculate aStar path when a corrupted byte falls on a position in the
--   path. if aStar fails return the corrupted byte.
partTwo :: Set Pos -> [Pos] -> Grid -> Pos
partTwo _ [] _ = error "no byte cuts off exit"
partTwo path (pos : corrupted) grid = do
    let grid' = M.insert pos Corrupted grid
    if S.notMember pos path
        then partTwo path corrupted grid'
        else case aStar grid' of
            Nothing -> pos
            Just endNode ->
                let path' = tracePathFrom endNode
                 in partTwo (S.fromList path') corrupted grid'

printGrid :: Grid -> IO ()
printGrid grid =
    -- swap (x, y) to (y, x) because indices in grid are represented in the
    -- fourth quadrant printed in the first.
    let ps = map swap positions
     in print' ps (fst endPos + 1)
    where
    print' [] _ = pure ()
    print' ps rowLength = do
        let (row, ps') = splitAt rowLength ps
        mapM_ (\p -> case grid M.! p of
                Corrupted -> putChar '#'
                Safe -> putChar '.'
                Traversed -> putChar 'O'
              ) row
        putChar '\n'
        print' ps' rowLength
