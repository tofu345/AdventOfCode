{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List

main = do
    contents <- B.lines <$> B.readFile "input.txt"
    let (Right data') = mapM (parseOnly parser) contents
        m = M.fromList data'
        ppl = nub $ map (fst . fst) data'

    putStr "Part One: "
    print $ simulate m ppl

    putStr "Part Two: "
    print $ simulate m ("me":ppl)

parser = do
    p1 <- takeTill isSpace
    stringCI " would "
    s <- takeTill isSpace
    skipSpace
    num <- decimal
    stringCI " happiness units by sitting next to "
    p2 <- takeTill (=='.')
    return ((p1, p2), sign s num)
    where
    sign "gain" n = n
    sign "lose" n = negate n

simulate m ppl = maximum
        $ map ((sum . f Nothing) . (\v -> last v : v))
        $ permutations ppl
    where
    f :: Maybe B.ByteString -> [B.ByteString] -> [Int]
    f _ [] = []
    f Nothing (x:xs) = f (Just x) xs
    f _ (cur@"me":xs) = 0 : f (Just cur) xs
    f (Just "me") (cur:xs) = 0 : f (Just cur) xs
    f (Just pre) (cur:xs) =
        m ! (pre, cur) : m ! (cur, pre) : f (Just cur) xs
