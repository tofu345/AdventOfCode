module Main where

import Data.Hash.MD5
import Data.String.Utils

salt = "bgvyzdsv"

main = do
    putStr "Part One: " 
    print $ until (\n -> startswith "00000" . md5s $ Str (salt ++ show n)) (+1) 0    

    putStr "Part Two: " 
    print $ until (\n -> startswith "000000" . md5s $ Str (salt ++ show n)) (+1) 0
