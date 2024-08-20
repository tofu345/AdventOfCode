{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.IO
import System.Exit
import Data.Foldable
import Data.Maybe
import Data.Ord
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Concurrent

initialState = State
    { playerHp = 50
    , playerMana = 500
    , bossHp = 58
    , bossDmg = 9
    -- { playerHp = 10
    -- , playerMana = 250
    -- , bossHp = 14
    -- , bossDmg = 8
    , spellsCast = []
    , effects = []
    }

main = do
    partOne initialState

data State = State
    { playerHp :: Int
    , playerMana :: Int
    , spellsCast :: [Spell]
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

spells = enumFrom MagicMissile

data Effect = Effect
    { name :: String
    , roundsLeft :: Int
    , effect :: State -> State
    }

instance Show Effect where
    show Effect {name} = name

instance Eq Effect where
    Effect {name = n1} == Effect {name = n2} = n1 == n2

instance Eq State where
    State {spellsCast = m1} == State {spellsCast = m2} = m1 == m2

instance Ord State where
    State {spellsCast = m1} <= State {spellsCast = m2} = map cost m1 <= map cost m2

getEffect Shield = Just shield
getEffect Poison = Just poison
getEffect Recharge = Just recharge
getEffect _ = Nothing

cost :: Spell -> Int
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

poison = Effect "Poison" 6
    (\v@State {bossHp} -> v {bossHp = bossHp - 3})
-- implemented elsewhere
shield = Effect "Shield" 6 id
recharge = Effect "Recharge" 5
    (\v@State {playerMana} -> v {playerMana = playerMana + 101})

run :: (State -> Maybe State) -> State -> Maybe State
run f state = do
    guard (playerMana state > 0 && playerHp state > 0)
    let state' = applyEffects state
    if bossHp state' <= 0
        then return state'
        else do
            st <- f state'
            guard (playerMana st > 0 && playerHp st > 0)
            return st

applyEffects :: State -> State
applyEffects d@State {effects} =
    let data' = foldl (flip effect) d effects
        effects' = map (\e@Effect{roundsLeft} ->
            e {roundsLeft = roundsLeft - 1}) effects
    in data'
        { effects = filter (\Effect {roundsLeft} -> roundsLeft > 0) effects' }

playerTurn :: Spell -> State -> Maybe State
playerTurn sp st@State{playerMana, spellsCast} = do
    st' <- cast sp $ st
        { playerMana = playerMana - cost sp
        , spellsCast = sp : spellsCast }
    run bossTurn st'
    where
    cast :: Spell -> State -> Maybe State
    cast MagicMissile st@State{bossHp} = Just $ st {bossHp = bossHp - 4}
    cast Drain st@State{bossHp, playerHp} = Just
        $ st {bossHp = bossHp - 2, playerHp = playerHp + 2}
    cast spell st@State{effects} =
        getEffect spell >>= (\e -> Just $ st {effects = e : effects})

bossTurn :: State -> Maybe State
bossTurn st = do
    let hasShield = shield `elem` effects st
        bossDmg' = if hasShield then max 1 (bossDmg st - 7) else bossDmg st
        st' = st {playerHp = playerHp st - bossDmg'}
    guard (playerMana st' > 0 && playerHp st' > 0)
    foldl (\acc v -> minMaybe acc $ run (playerTurn v) st') Nothing spells

minMaybe :: Maybe State -> Maybe State -> Maybe State
minMaybe (Just st1) (Just st2) = Just $ min st1 st2
minMaybe (Just st1) Nothing = Just st1
minMaybe Nothing (Just st2) = Just st2
minMaybe _ _ = Nothing

partOne :: State -> IO ()
partOne s = do
    putStr "Part One: "
    -- let res = foldl (\acc v -> minMaybe acc $ run (playerTurn v) s) Nothing spells
    let res = run (playerTurn Poison) s
    case res of
        Just v -> do
            print $ reverse (spellsCast v)
            putStr "Mana Spent = "
            print $ sum $ map cost $ spellsCast v
        Nothing -> putStrLn "failed"
    return ()

-- partTwo ::
