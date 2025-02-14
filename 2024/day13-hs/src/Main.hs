module Main (main) where

import Text.Regex.Posix
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let regexExp = ".*X\\+([0-9]+), Y\\+([0-9]+)\n.*X\\+([0-9]+), Y\\+([0-9]+)\n.*X=([0-9]+), Y=([0-9]+)\n"
        data' = arrange <$> (contents =~ regexExp :: [[String]])
        arrange [_, xa, ya, xb, yb, x, y] = ((read x, read y), (read xa, read ya), (read xb, read yb))
        arrange _ = error "invalid data"

    putStr "Part One: "
    print $ solve $ data'

    putStr "Part Twe: "
    print $ solve $ p2 <$> data'

type Config = ((Double, Double), (Double, Double), (Double, Double))

p2 :: Config -> Config
p2 ((x, y), (xA, yA), (xB, yB)) = ((x + offset, y + offset), (xA, yA), (xB, yB))
    where
    offset = 10000000000000

solve :: [Config] -> Int
solve = sum . mapMaybe f
    where
    trunc :: Double -> Double
    trunc = fromIntegral . round
    f ((x, y), (xA, yA), (xB, yB)) =
        -- from simultaneous equations
        let stepsA = (y / yB - x / xB) / (yA / yB - xA / xB)
            stepsB = (x - xA * stepsA) / xB
            (sA, sB) = (trunc stepsA, trunc stepsB)
         in if xA * sA + xB * sB == x && yA * sA + yB * sB == y
                then Just $ round $ sA * 3 + sB
                else Nothing
