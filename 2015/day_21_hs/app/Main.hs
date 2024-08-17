{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString as B
import GHC.IO (mkUserError)
import Control.Exception (throw)
import Control.Monad (foldM, guard)
import Data.List (subsequences)

-- main :: IO ()
main = do
    contents <- B.readFile "input.txt"
    let boss = case parseOnly parser contents of
            Left _ -> throw (userError "invalid data")
            Right v -> v
    partOne boss
    partTwo boss
    return boss

parser :: Parser Boss
parser = do
    stringCI "Hit Points: "
    hp <- decimal
    endOfLine
    stringCI "Damage: "
    dmg <- decimal
    endOfLine
    stringCI "Armor: "
    def <- decimal
    endOfLine
    return (hp, dmg, def)

playerHitPoints = 100
weapons = [ item 8 4 0
          , item 10 5 0
          , item 25 6 0
          , item 40 7 0
          , item 74 8 0
          ]
armors = [ item 0 0 0
         , item 13 0 1
         , item 31 0 2
         , item 53 0 3
         , item 75 0 4
         , item 102 0 5
         ]
rings = [ item 25 1 0
        , item 50 2 0
        , item 100 3 0
        , item 20 0 1
        , item 40 0 2
        , item 80 0 3
        ]

data Item = Item
    { cost :: Int
    , damage :: Int
    , defence :: Int
    } deriving (Show, Eq)
-- (bossHp, bossDmg, bossDef)
type Boss = (Int, Int, Int)
data Player = Player
    { weapon :: Item
    , armor :: Item
    , ring :: [Item]
    } deriving (Show)

item cost damage defence = Item {cost, damage, defence}

instance Num Item where
    Item {cost = cost1, damage = dmg1, defence = def1} +
        Item {cost = cost2, damage = dmg2, defence = def2} =
            item (cost1 + cost2) (dmg1 + dmg2) (def1 + def2)

combinations :: [Player]
combinations = do
    weapon <- weapons
    armor <- armors
    ring <- filter (\v -> length v <= 2) (subsequences rings)
    return Player {weapon, armor, ring}

type Cost = Int
type Win = Bool
simulate :: Player -> Boss -> (Win, Cost)
simulate Player {weapon, armor, ring} (bossHp, bossDmg, bossDef) =
    let playerDmg = damage weapon + sum (map damage ring)
        playerDef = defence armor + sum (map defence ring)
        playerCost = cost weapon + cost armor + sum (map cost ring)
        dmgDealtToBoss = max (playerDmg - bossDef) 1
        dmgDealtToPlayer = max (bossDmg - playerDef) 1
        roundsToKill = ceiling $ toFloat bossHp / toFloat dmgDealtToBoss
        roundsSurvived = ceiling $ toFloat playerHitPoints / toFloat dmgDealtToPlayer
    in (roundsSurvived >= roundsToKill, playerCost)
    where toFloat x = fromIntegral x :: Float

partOne :: Boss -> IO ()
partOne boss = do
    minCost <-
        foldM (\acc v -> do
                let (win, cost) = simulate v boss
                if win && (acc == 0 || cost < acc)
                    then return cost
                    else return acc)
            0
            combinations
    putStr "Part One: "
    print minCost
    return ()

partTwo :: Boss -> IO ()
partTwo boss = do
    minCost <-
        foldM (\acc v -> do
                let (win, cost) = simulate v boss
                if not win && (acc == 0 || cost > acc)
                    then return cost
                    else return acc)
            0
            combinations
    putStr "Part Two: "
    print minCost
    return ()
