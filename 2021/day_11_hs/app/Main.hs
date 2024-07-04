module Main ( main ) where

import System.Exit
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad

main :: IO ()
main = do
    matrix <- map (map digitToInt) . lines <$> readFile "input.txt"
    let dimensions = (length (head matrix), length matrix)
    let points = Map.fromList
            $ concatMap
            (\(y, row) -> zipWith (\x v -> (Point x y, v)) [0..] row)
            $ zip [0 ..] matrix
    partOne points dimensions
    partTwo points dimensions
    return ()

type Points = Map.Map Point Int

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

adjacent Point {x=x, y=y} =
            [ Point {x = x, y = y - 1}
            , Point {x = x + 1, y = y}
            , Point {x = x, y = y + 1}
            , Point {x = x - 1, y = y}
            , Point {x = x - 1, y = y - 1}
            , Point {x = x + 1, y = y + 1}
            , Point {x = x + 1, y = y - 1}
            , Point {x = x - 1, y = y + 1}
            ]

simulate :: Points -> Points
simulate points =
    let points' = fmap (+1) points
    in foldl recurse points' (Map.keys points')
    where
        recurse :: Points -> Point -> Points
        recurse points curr_p
            | Just val <- Map.lookup curr_p points =
                if val > 9
                    then foldl
                            (\acc next_p ->
                                case Map.lookup next_p acc of
                                    Just next_v | next_v /= 0 ->
                                        recurse (Map.adjust (+1) next_p acc) next_p
                                    _ -> acc)
                            (Map.adjust (const 0) curr_p points)
                            (adjacent curr_p)
                    else points
            | otherwise = points

printPoints :: Points -> (Int, Int) -> IO ()
printPoints p (x_max, y_max) = do
    mapM_ (\y -> do
        mapM_ (\x ->
            putStr . show . fromJust
            $ Map.lookup (Point {x=x, y=y}) p)
            [0 .. x_max-1]
        putStrLn "")
        [0 .. y_max-1]
    putStrLn ""

partOne :: Points -> (Int, Int) -> IO ()
partOne points dimensions = do
    (res, num_flashes) <-
            foldM (\(acc, flashes) _ -> do
                let res = simulate acc
                    flashes' = flashes +
                        (length . filter (==0)
                        $ Map.elems res)
                -- printPoints res dimensions
                return (res, flashes'))
            (points, 0)
            [1..100]
    putStr "Part One: "
    print num_flashes

partTwo :: Points -> (Int, Int) -> IO ()
partTwo points dimensions = do
    foldM_ 
        (\acc idx -> do
            let res = simulate acc
            if all (==0) $ Map.elems res
               then putStrLn ("Part Two: " ++ show idx) >> exitSuccess
               else return res)
        points
        [1..]
    putStrLn "Part Two: fail"
