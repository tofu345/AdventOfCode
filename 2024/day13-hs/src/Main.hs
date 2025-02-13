{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Maybe (mapMaybe)
import Data.Either (fromRight)

main :: IO ()
main = do
    contents <- splitN 4 . B.lines <$> B.readFile "input.txt"
    let data' = fromRight (error "invalid data")
              $ mapM (parseOnly parser) contents

    putStr "Part One: "
    print $ solve $ data'

    putStr "Part Twe: "
    print $ solve $ p2 <$> data'

p2 ((x, y), (xA, yA), (xB, yB)) = ((x + offset, y + offset), (xA, yA), (xB, yB))
    where
    offset = 10000000000000

splitN n xs | null xs = []
            | otherwise = let (l, r) = splitAt n xs
                           in B.unlines l : splitN n r

parser = do
    stringCI "Button A: X+"
    xA <- double
    stringCI ", Y+"
    yA <- double
    endOfLine

    stringCI "Button B: X+"
    xB <- double
    stringCI ", Y+"
    yB <- double
    endOfLine

    stringCI "Prize: X="
    x <- double
    stringCI ", Y="
    y <- double
    endOfLine
    return ((x, y), (xA, yA), (xB, yB))

solve = sum . mapMaybe f
    where
    trunc = fromIntegral . round
    f ((x, y), (xA, yA), (xB, yB)) =
        -- from simultaneous equations
        let stepsA = (y / yB - x / xB) / (yA / yB - xA / xB)
            stepsB = (x - xA * stepsA) / xB
            (sA, sB) = (trunc stepsA, trunc stepsB)
            tokens = sA * 3 + sB
         in if xA * sA + xB * sB == x && yA * sA + yB * sB == y
                then Just $ round $ sA * 3 + sB
                else Nothing
