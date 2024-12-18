{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ord
import Data.Function
import Data.List
import Data.Maybe
import Data.Either
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    contents <- B.lines <$> B.readFile "input.txt"
    let data' = fromRight (error "invalid data")
              $ mapM (parseOnly parser) contents
        realRooms = filter isRealRoom data'

    putStr "Part One: "
    print $ sum $ map (\(_, i, _) -> i) realRooms

    putStr "Part Two: "
    case find (("north" `isPrefixOf`) . fst) $ map decrypt data' of
        Nothing -> putStrLn "fail"
        Just (_, i) -> print i

    where
    shiftChar _ 'z' = 'e'
    shiftChar _ '-' = ' '
    shiftChar n v = last $ Data.List.take n $ iterate shift' v

    shift' 'z' = 'a'
    shift' v = succ v

    decrypt (s, id, c) =
        let n = (id `mod` 26) + 1
         in (map (shiftChar n) s, id)

mostCommonLetters :: String -> String
mostCommonLetters s =
    let m = foldl f M.empty (filter (/= '-') s)
     in map fst $ sortBy (compare `on` (Down . snd)) $ M.toList m
    where f acc v = M.insertWith (+) v 1 acc

isRealRoom l@(str, _, check) =
    check `isPrefixOf` mostCommonLetters str

parser = do
    name <- B.init <$> takeTill isDigit
    idStr <- takeTill (=='[')
    let (id, _) = fromMaybe (error "invalid data") (B.readInt idStr)
    stringCI "["
    checksum <- takeTill (==']')
    return (B.unpack name, id, B.unpack checksum)
