module Main where

import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.ByteString.Base16 (encode)
import Data.List

-- my saviour...
-- https://www.reddit.com/r/adventofcode/comments/5i8pzz/comment/db6id3x/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

salt :: String
salt = "zpqevtbw"

main :: IO ()
main = do
    let hashes = [(x, hash' 1 x) | x <- [0..]]
    putStr "Part One: "
    print $ fst $ valid hashes !! 63

    let hashes' = [(x, hash' 2017 x) | x <- [0..]]
    putStr "Part Two: "
    print $ fst $ valid hashes' !! 63

    where
    hash' :: Int -> Int -> ByteString
    hash' stretches idx = iterate (encode . hash) (pack $ salt ++ show idx) !! stretches

    valid :: [(Int, ByteString)] -> [(Int, ByteString)]
    valid [] = []
    valid ((idx, cur):rest)
        | Just threes <- find ((>=3) . B.length) (B.group cur) =
            case find (containsFive threes) (take 1000 rest) of
                Nothing -> valid rest
                Just _ -> (idx, cur) : valid rest
        | otherwise = valid rest

    containsFive threes (_, h) = B.replicate 5 (B.head threes) `B.isInfixOf` h

