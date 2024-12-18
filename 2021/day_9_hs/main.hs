import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Monoid

main = do
    matrix <- map (map digitToInt) . lines <$> readFile "input.txt"
    let points = concatMap (\(y, row) -> map (`Point` y) [0 .. length row - 1]) $ zip [0 ..] matrix
    partOne matrix points
    partTwo matrix points

type Matrix = [[Int]]

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

at :: Matrix -> Point -> Maybe Int
at [] _ = Nothing
at m Point {x, y}
    | x < 0 || x >= length (head m) || y < 0 || y >= length m = Nothing
    | otherwise = Just $ m !! y !! x

neighbours Point {x, y} =
            [ Point {x, y = y - 1}
            , Point {x = x + 1, y}
            , Point {x, y = y + 1}
            , Point {x = x - 1, y}
            ]

neighbours' :: Matrix -> Point -> [Int]
neighbours' m p = mapMaybe (at m) $ neighbours p

partOne :: Matrix -> [Point] -> IO ()
partOne matrix points = do
    let risks = foldl (\acc curr ->
            let val = fromJust $ matrix `at` curr
            -- in if all (val <) $ neighbours' matrix curr
            in if getAll $ foldMap (All . (val <)) $ neighbours' matrix curr
                then val + 1 : acc
                else acc)
            []
            points
    putStrLn $ "Part One: " ++ show (sum risks)

findBasin :: Matrix -> Point -> [Point] -> [Point]
findBasin m curr_p basin
    | curr_p `elem` basin = basin
    | Just val <- m `at` curr_p =
        if val == 9
            then basin
            else foldl
                     (\basin' next_p ->
                          case m `at` next_p of
                              Nothing -> basin'
                              Just next -> findBasin m next_p basin')
                     (curr_p : basin)
                     (neighbours curr_p)
    | otherwise = basin

lowPoint :: Matrix -> Point -> Bool
lowPoint m p = all ((fromJust $ m `at` p) <) $ neighbours' m p

partTwo :: Matrix -> [Point] -> IO ()
partTwo matrix points = do
    let basins = map (\p -> findBasin matrix p []) $
            filter (lowPoint matrix) points
    let basinLengths = sortBy (comparing Down) . map length $ basins
    case basinLengths of
        [] -> putStrLn "incorrect data or fail"
        a:b:c:_ -> putStrLn $ "Part Two: " ++ show (a * b * c)
