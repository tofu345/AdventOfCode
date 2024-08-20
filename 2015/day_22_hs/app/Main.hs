{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad

initialState = State
    { playerHp = 50
    , playerMana = 500
    , bossHp = 58
    , bossDmg = 9
    , manaSpent = 0
    , effects = []
    }

main = do
    partOne
    partTwo

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

spells = enumFrom MagicMissile

data Effect = Effect
    { name :: String
    , rounds :: Int
    , effect :: State -> State
    }

instance Show Effect where
    show Effect {name, rounds} = name ++ "(" ++ show rounds ++ ")"

instance Eq Effect where
    Effect {name = n1} == Effect {name = n2} = n1 == n2

instance Eq State where
    State {manaSpent = m1, effects = e1}
        == State {manaSpent = m2, effects = e2} =
        m1 == m2 && e1 == e2

instance Ord State where
    State {manaSpent = m1} <= State {manaSpent = m2} = m1 <= m2

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

cast :: Spell -> State -> Maybe State
cast MagicMissile st@State{bossHp} = Just st {bossHp = bossHp - 4}
cast Drain st@State{bossHp, playerHp} = Just st
    {bossHp = bossHp - 2, playerHp = playerHp + 2}
cast spell st@State{effects} = do
    effect <- getEffect spell
    guard (effect `notElem` effects)
    Just st {effects = effect : effects}

easy :: State -> Maybe State
easy st@State {effects} =
    let st' = foldl (flip effect) st effects
        effects' =
            map (\e@Effect{rounds} -> e {rounds = rounds - 1})
            effects
    in Just st' { effects =
            filter (\Effect {rounds} -> rounds >= 1) effects' }

hard :: State -> Maybe State
hard st@State {effects, playerHp} =
    let st' = foldl
            (flip effect)
            st {playerHp = playerHp - 1}
            effects
        effects' =
            map (\e@Effect{rounds} -> e {rounds = rounds - 1})
            effects
    in Just st' { effects =
            filter (\Effect {rounds} -> rounds >= 1) effects' }

player :: (State -> Maybe State) -> Spell -> State -> Maybe State
player p sp st = do
    check p st
        >>= check (\s@State {playerMana, manaSpent} -> cast sp
                $ s { playerMana = playerMana - cost sp
                    , manaSpent = manaSpent + cost sp })
        >>= check (boss p)

boss :: (State -> Maybe State) -> State -> Maybe State
boss p st = do
    let bossDmg' = if shield `elem` effects st
                       then max 1 (bossDmg st - 7)
                       else bossDmg st
    check p st
        >>= check (\s@State{playerHp} ->
                Just s {playerHp = playerHp - bossDmg'})
        >>= check (\s -> foldl
                (\acc spell -> minMaybe acc (player p spell s))
                    Nothing
                    spells)

minMaybe :: Maybe State -> Maybe State -> Maybe State
minMaybe (Just st1) (Just st2) = Just $ min st1 st2
minMaybe (Just st1) Nothing = Just st1
minMaybe Nothing (Just st2) = Just st2
minMaybe _ _ = Nothing

partOne = do
    putStr "Part One: "
    -- let res = foldl
    --         (\acc spell -> minMaybe acc $ player spell s)
    --         Nothing spells
    let res = player easy Poison initialState
    case res of
        Just st -> do
            print $ manaSpent st
        Nothing -> putStrLn "failed"

partTwo = do
    putStr "Part Two: "
    let res = player hard Poison initialState
    case res of
        Just st -> do
            print $ manaSpent st
        Nothing -> putStrLn "failed"
