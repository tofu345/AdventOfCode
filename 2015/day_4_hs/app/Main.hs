module Main where

import Data.Hash.MD5
import Data.String.Utils

main :: IO ()
main = do
    putStr "Part One: " >> print (partOne 0)
    putStr "Part Two: " >> print (partTwo 0)

salt = "bgvyzdsv"

partOne num = do
    if startswith "00000" 
        $ md5s (Str $ salt ++ show num)
        then num
        else partOne (num + 1)

partTwo num = do
    if startswith "000000" 
        $ md5s (Str $ salt ++ show num)
        then num
        else partTwo (num + 1)
