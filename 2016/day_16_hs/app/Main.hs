{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

main :: IO ()
main = do
    let initial =  "01000100010010111"
    putStrLn $ "Part One: " ++ B.unpack (solve 272 initial)
    putStrLn $ "Part Two: " ++ B.unpack (solve 35651584 initial)

genRandomData :: ByteString -> ByteString
genRandomData str =
    (str `B.snoc` '0') `B.append` B.map invert (B.reverse str)
    where invert '0' = '1'
          invert '1' = '0'
          invert _ = error "invalid data"

checksum = B.unfoldr f
    where
    f str = do
        (x, str') <- B.uncons str
        (y, str'') <- B.uncons str'
        return (if x == y then '1' else '0', str'')

solve :: Int -> ByteString -> ByteString
solve len str =
    let s = until ((>= len) . B.length) genRandomData str
     in until (odd . B.length) checksum (B.take len s)
