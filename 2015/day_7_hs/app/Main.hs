module Main where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Bits
import Data.Char (isDigit)
import Data.Word (Word16)
import Data.Maybe (isNothing, fromJust)
import Text.Read (readMaybe)
import Data.Function.Memoize

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let hm = M.fromList
            $ map (\line -> do
                    let [src, dest] = splitOn " -> " line
                    (dest, words src))
                $ lines contents

    let wireA = fromJust $ solve hm "a"
    putStr "Part One: " >> print wireA

    let newHm = M.insert "b" [show wireA] hm
    let wireA2 = fromJust $ solve newHm "a"
    putStr "Part Two: " >> print wireA2

type Wires = M.Map String [String]
type Target = String

solve :: Wires -> Target -> Maybe Word16
solve hm target = M.lookup target hm >>= eval'
    where
        eval' = memoize eval

        eval :: [String] -> Maybe Word16
        eval ["NOT", v] = complement <$> eval' [v]
        eval [l, "AND", r] = (.&.) <$> eval' [l] <*> eval' [r]
        eval [l, "OR", r] = (.|.) <$> eval' [l] <*> eval' [r]
        eval [l, "LSHIFT", count] = (`shiftL` read count) <$> eval' [l]
        eval [l, "RSHIFT", count] = (`shiftR` read count) <$> eval' [l]
        eval [v] =
            if isDigit (head v)
                then readMaybe v
                else do
                    eval' =<< M.lookup v hm
