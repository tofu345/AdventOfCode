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
    -- { playerHp = 50
    -- , playerMana = 500
    -- , bossHp = 58
    -- , bossDmg = 9
    { playerHp = 10
    , playerMana = 250
    , bossHp = 14
    , bossDmg = 8
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
    show Effect {name, roundsLeft} = name ++ "(" ++ show roundsLeft ++ ")"

instance Eq Effect where
    Effect {name = n1} == Effect {name = n2} = n1 == n2

instance Eq State where
    State {spellsCast = m1, effects = e1}
        == State {spellsCast = m2, effects = e2} =
        sum (map cost m1) == sum (map cost m2) && e1 == e2

instance Ord State where
    State {spellsCast = m1} <= State {spellsCast = m2} =
        sum (map cost m1) <= sum (map cost m2)

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

poison = Effect "poison" 6
    (\v@State {bossHp} -> v {bossHp = bossHp - 3})
-- implemented elsewhere
shield = Effect "shield" 6 id
recharge = Effect "recharge" 5
    (\v@State {playerMana} -> v {playerMana = playerMana + 101})

check :: (State -> Maybe State) -> State -> Maybe State
check f st@State{bossHp, playerHp, playerMana}
    | bossHp <= 0 = return st
    | playerHp <= 0 || playerMana <= 0 = Nothing
    | otherwise = f st

applyEffects :: State -> State
applyEffects st@State {effects} =
    let st' = foldl (flip effect) st effects
        effects' = map (\e@Effect{roundsLeft} -> e {roundsLeft = roundsLeft - 1}) effects
    in st' { effects = filter (\Effect {roundsLeft} -> roundsLeft >= 1) effects' }

player :: Spell -> State -> Maybe State
player sp st = do
    check (Just . applyEffects) st
        >>= check (\s@State {playerMana, spellsCast} -> cast sp
            $ s { playerMana = playerMana - cost sp
                , spellsCast = sp : spellsCast })
        >>= check boss
    where
    cast :: Spell -> State -> Maybe State
    cast MagicMissile st@State{bossHp} = Just st {bossHp = bossHp - 4}
    cast Drain st@State{bossHp, playerHp} = Just st
        {bossHp = bossHp - 2, playerHp = playerHp + 2}
    cast spell st@State{effects} = do
        effect <- getEffect sp
        guard (effect `notElem` effects)
        Just st {effects = fromJust (getEffect spell) : effects}

boss :: State -> Maybe State
boss st = do
    let hasShield = shield `elem` effects st
        bossDmg' = if hasShield then max 1 (bossDmg st - 7) else bossDmg st
    check (Just . applyEffects) st
        >>= check (\s@State{playerHp} -> Just s {playerHp = playerHp - bossDmg'})
        >>= check (\s -> foldl
                (\acc spell -> minMaybe acc (player spell s))
                Nothing
                spells)

minMaybe :: Maybe State -> Maybe State -> Maybe State
minMaybe (Just st1) (Just st2) = Just $ min st1 st2
minMaybe (Just st1) Nothing = Just st1
minMaybe Nothing (Just st2) = Just st2
minMaybe _ _ = Nothing

partOne :: State -> IO ()
partOne s = do
    putStr "Part One: "
    putStrLn ""
    let res = foldl
            (\acc spell -> minMaybe acc $ player spell s)
            Nothing spells
    -- let res = player Recharge s
    case res of
        Just st -> do
            print $ reverse (spellsCast st)
            putStr "Mana Spent = "
            print $ sum $ map cost $ spellsCast st
        Nothing -> putStrLn "failed"
    -- return ()

-- partTwo ::
