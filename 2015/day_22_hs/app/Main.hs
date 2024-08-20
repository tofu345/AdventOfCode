{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.Exit
import Data.Foldable
import Data.Maybe
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent
import System.IO

initialState = State
    { playerHp = 50
    , playerMana = 500
    , bossHp = 58
    , bossDmg = 9
    -- { playerHp = 10
    -- , playerMana = 250
    -- , bossHp = 13
    -- , bossDmg = 8
    , manaSpent = 0
    , effects = []
    }

main = do
    partOne initialState

data State = State
    { playerHp :: Int
    , playerMana :: Int
    , manaSpent :: Int
    , effects :: [Effect]
    , bossHp :: Int
    , bossDmg :: Int
    } deriving (Show)

data Spell
    = MagicMissile
    | Drain
    | Shield
    | Poison
    | Recharge
    deriving (Show, Eq, Enum)

data Effect = Effect
    { name :: String
    , effectStr :: String
    , roundsLeft :: Int
    , effect :: State -> State
    }

instance Show Effect where
    show Effect {name} = name

instance Eq Effect where
    Effect {name = n1} == Effect {name = n2} = n1 == n2

getEffect Shield = shield
getEffect Poison = poison
getEffect Recharge = recharge
getEffect _ = error "spell has no effect"

hasEffect s = s `elem` [Shield, Poison, Recharge]

cost :: Spell -> Int
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

poison = Effect "Poison" "deals 3 damage" 6 
    (\v@State {bossHp} -> v {bossHp = bossHp - 3})
-- implemented elsewhere
shield = Effect "Shield" "provides 7 armor" 6 id
recharge = Effect "Recharge" "provides 101 mana" 5 
    (\v@State {playerMana} -> v {playerMana = playerMana + 101})

printState :: State -> IO State
printState s@State{playerHp, playerMana, bossHp, bossDmg} = do
    putStrLn $ "\n-- Player has " ++ show playerHp
        ++ " hit points, " ++ show playerMana ++ " mana"
    putStrLn $ "-- Boss has " ++ show bossHp
        ++ " hit points, deals " ++ show bossDmg ++ " damage"
    return s

run :: (State -> IO ()) -> State -> IO ()
run f st = do
    threadDelay 1000000
    end <- gameOver st
    unless end $ do
        printState st
        st' <- applyEffects st
        end' <- gameOver st'
        unless end' (f st')
    where
    gameOver :: State -> IO Bool
    gameOver State {bossHp, playerHp, playerMana, manaSpent}
        | bossHp <= 0 = do
            putStrLn $ "Boss Defeated; Spent "
                ++ show manaSpent ++ " mana"
            return True
        | playerHp <= 0 = do
            putStrLn "Player Defeated"
            return True
        | playerMana <= 0 = do
            putStrLn "Player Mana Depleted"
            return True
        | otherwise = return False

applyEffects :: State -> IO State
applyEffects d@State {effects} = do
    let data' = foldl (flip effect) d effects
    forM_ effects
        (\e -> putStrLn
            $ name e ++ " "
            ++ effectStr e
            ++ "; its timer is now "
            ++ show (roundsLeft e - 1))
    let effects' = map (\e@Effect{roundsLeft} ->
            e {roundsLeft = roundsLeft - 1}) effects
    return $ data'
        { effects =
            filter
                (\Effect {roundsLeft} -> roundsLeft > 0)
                effects' }

playerTurn :: State -> IO ()
playerTurn st@State{playerMana, manaSpent} = do
    spell <- chooseSpell
    st' <- cast spell $ st
        { playerMana = playerMana - cost spell
        , manaSpent = manaSpent + cost spell }
    run bossTurn st'
    where
    cast :: Spell -> State -> IO State
    cast MagicMissile st@State{bossHp} =
        putStrLn "Player casts Magic Missile, dealing 4 damage"
        >> return (st {bossHp = bossHp - 4})
    cast Drain st@State{bossHp, playerHp} =
        putStrLn "Player casts Drain, dealing 2 damage, and healing 2 hit points"
        >> return (st {bossHp = bossHp - 2, playerHp = playerHp + 2})
    cast spell st@State{effects} =
        let e = getEffect spell
        in do
            when (e `elem` effects) $ error "duplicate effect"
            putStrLn $ "Player casts " ++ name e
            return (st {effects = e : effects})

bossTurn :: State -> IO ()
bossTurn st@State {effects, playerHp} = do
    let hasShield = shield `elem` effects
    let bossDmg' = if hasShield then max 1 (bossDmg st - 7) else bossDmg st
    putStr "Boss attacks for "
    if hasShield
        then putStrLn $ show (bossDmg st) ++ " - 7 = " ++ show bossDmg'
        else print bossDmg'
    run playerTurn (st {playerHp = playerHp - bossDmg'})

chooseSpell :: IO Spell
chooseSpell = do
    hSetBuffering stdout NoBuffering
    putStr "Choose: [m]agicMissile, [d]ain, [s]hield, [p]oison, [r]echarge: "
    input <- getLine
    case head input of
        'm' -> return MagicMissile
        'd' -> return Drain
        's' -> return Shield
        'p' -> return Poison
        'r' -> return Recharge
        _ -> error "invalid input"

partOne :: State -> IO ()
partOne s = do
    putStrLn "Spells:"
    putStrLn $ "Magic Missile: " ++ show (cost MagicMissile) ++ " mana, deals 4 damage instantly"
    putStrLn $ "Drain: " ++ show (cost Drain) ++ " mana, deals 2 damage, heals 2 hit points"
    putStrLn $ "Shield: " ++ show (cost Shield) ++ " mana, reduces bossDmg by 7 for 6 turns"
    putStrLn $ "Poison: " ++ show (cost Poison) ++ " mana, deals 3 dmg at the start of 6 turns"
    putStrLn $ "Recharge: " ++ show (cost Recharge) ++ " mana, gives 101 mana at the start of 5 turns"

    run playerTurn s
    return ()

-- partTwo ::
