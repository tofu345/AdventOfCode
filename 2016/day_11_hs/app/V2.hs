{-# LANGUAGE NamedFieldPuns #-}

module V2 where

import Data.Bifunctor
import Data.List
import Data.Function
import Data.Semigroup
import Control.Monad

type Pairs = ([[String]], [String])
type Result = Maybe State
type Floor = [String]

data State = State
    { curr :: [String]
    , up :: [Floor]
    , down :: [Floor]
    , steps :: Int
    } deriving (Show)

group' :: Floor -> Pairs
group' flr = second concat
    $ partition ((==2) . length)
    $ groupBy ((==) `on` init)
    $ sort flr

moveUp State {curr, up, down, steps} val = State
    { curr = head up ++ val
    , up = tail up
    , down = down ++ [filter (`notElem` val) curr]
    , steps = steps + 1 }

moveDown State {curr, up, down, steps} val = State
    { curr = head down ++ val
    , up = up ++ [filter (`notElem` val) curr]
    , down = tail down
    , steps = steps + 1 }

go :: State -> Maybe State
go st@State {curr, up, down, steps}
    | null up && all null down = Just st
    | otherwise = do
        let (pairs, nonPairs) = group' curr
        guard $ not $ any ((== 'M') . last) nonPairs && not (null pairs)
        let combn = pairs ++ tail (subsequences nonPairs)
            st' = case up of [] -> Nothing 
                             _ -> foldl' (f moveUp) Nothing combn
        case down of 
            [] -> st'
            _ -> foldl' (f moveDown) st' combn
        where
        f mvDir acc val = min' acc $ go (mvDir st val)

        canMove up' down' [] = (up', down')
        canMove up' down' (p:xs)
            | noGeneratorsTill (findPair p) up = canMove (p : up') down' xs
            | noGeneratorsTill (findPair p) down = canMove up' (p : down') xs

        noGeneratorsTill :: String -> [Floor] -> Bool
        noGeneratorsTill p f =
            let (_, flrs) = break (p `elem`) f
            -- works but I dont know how.  ¯\_(ツ)_/¯ $ if it aint broke, don't what?
             in not (all null flrs)
                || any (p `elem`) f && not (generator flrs)

generator = any (any $ ('G' ==) . last)

findPair xs =
     if last xs == 'G'
         then init xs ++ "M"
         else init xs ++ "G"

min' (Just s1) (Just s2) =
    Just $ if steps s1 > steps s2 then s2 else s1
min' Nothing (Just s) = Just s
min' (Just s) Nothing = Just s
min' Nothing Nothing = Nothing

testData' = [["MG"], [], [], ["HM", "LM", "HG", "LG"]]

run :: IO ()
run = do
    let (curr:up) = testData'
        res = go State { curr, down = [], up, steps = 0 }
    print res
    return ()
