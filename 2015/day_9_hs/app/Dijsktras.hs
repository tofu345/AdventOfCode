{-# LANGUAGE NamedFieldPuns #-}

module Dijsktras where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List
import Control.Monad (foldM)

main cons = do
    let cities = nub $ M.keys cons
        per = permutations cities

    let t = cities
    print t
    dijsktras cons (head t) (last t) t
    return ()

-- does not go through all cities since its faster not to :|

data Node = Node
    { name :: String
    , dist :: Int
    , from :: Maybe Node
    } deriving (Show)

instance Eq Node where
    Node {name = n1} == Node {name = n2} = n1 == n2

-- for insert fn
instance Ord Node where
    Node {dist = d1} <= Node {dist = d2} = d1 <= d2

dijsktras :: M.Map String [(String, Int)] -> String -> String -> [String] -> IO ()
dijsktras cons start end cities = do
    let r = run [Node start 0 Nothing] []
    print (head r)
    where
    run :: [Node] -> [String] -> [Node]
    run [] _ = []
    run (curr@Node {name, dist}:queue) done
        | name == end = curr : queue
        | otherwise = do
            let nodes = cons ! name
            let q = foldl (f done curr) queue nodes
            run q (name:done)

    f :: [String] -> Node -> [Node] -> (String, Int) -> [Node]
    f done prev acc (curr, currDist)
        | curr `elem` done || curr == start = acc
        | otherwise = do
            let dist' = dist prev + currDist
                next = Node curr dist' $ Just prev
            case find ((== curr) . name) acc of
                Nothing -> insert next acc
                Just c -> do
                    if dist c < dist'
                        then acc
                        else do insert next (delete c acc)
