module Main where

import Data.Hash.MD5
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

salt = "cxdnnyjw"

main = do
    putStrLn "2m30s run time :<"

    let ans1 = map head $ take 8 $ mapMaybe p1 [1..]
    putStrLn $ "Part One: " ++ ans1
    putStrLn $ "Part Two: " ++ getPass 1 (replicate 8 '_')

p1 i = stripPrefix "00000" $ md5s (Str $ salt ++ show i)

getPass :: Int -> String -> String
getPass idx pass = do
    let pass' = fromMaybe pass $ hash idx
    if '_' `elem` pass'
        then getPass (idx + 1) pass'
        else pass'
    where
    hash :: Int -> Maybe String
    hash idx = do
        let h = md5s (Str $ salt ++ show idx)
        (p:v:_) <- stripPrefix "00000" h
        guard (isOctDigit p)
        let (pre, c:suf) = splitAt (digitToInt p) pass
        guard (c == '_')
        return $ pre ++ [v] ++ suf
