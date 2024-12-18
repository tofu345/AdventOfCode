{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Writer
import Control.Applicative
import Data.List
import Data.Function
import qualified Data.DList as D
import Data.Maybe
import Data.Bifunctor

import qualified V2

testData' = [["HM", "LM"], ["HG"], ["LG"], []]
inputData = [["ThG", "ThM", "PlG", "StG"], ["PlM", "StM"], ["PrG", "PrM", "RuG", "RuM"], []]

-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.

main = do
    -- mapM_ putStrLn $ showFloors testData'
    let (flr:xs) = inputData
        (pairs, nonPairs) = group' flr
        -- (v, logs) = runWriter $ unpaired State {pairs, nonPairs, down = [], up = xs, steps = 0}
    -- mapM_ putStrLn logs
    -- putStrLn $ "Ans: " ++ maybe "Failed, you suck." show' v
    -- mapM_ print testData'

    V2.run

    return ()

type Pairs = ([[String]], [String])
type Result = Writer (D.DList String) (Maybe State)
type Floor = [String]
data Dir = Up | Down deriving (Show)
data State = State
    { pairs :: [[String]]
    , nonPairs :: [String]
    , up :: [Floor]
    , down :: [Floor]
    , steps :: Int
    } deriving (Show)

-- TODO: Use Zipper

instance Semigroup State where
    s1@State {steps = steps1} <> s2@State {steps = steps2} =
        if steps1 > steps2
            then s2
            else s1

findPair xs =
     if last xs == 'G'
         then init xs ++ "M"
         else init xs ++ "G"

-- hashmap?
group' :: Floor -> Pairs
group' flr = second concat
    $ partition ((==2) . length)
    $ groupBy ((==) `on` init)
    $ sort flr

showFloors flr = map
        (\(i, v) -> "floor " ++ show i ++ " " ++ show v)
        $ reverse (zip [1..] flr)

show' State {pairs, nonPairs, down, up, steps} =
    show (concat pairs ++ nonPairs)
    ++ " up: " ++ show up
    ++ " down: " ++ show down
    ++ " taken: " ++ show steps ++ " steps"

takeTillIf fn = f []
    where
    f _ [] = []
    f prev (x:xs) =
         if fn x
            then reverse prev
            else f (x : prev) xs

generator = any (any $ ('G' ==) . last)
moleculeNot = any (any $ ('M' /=) . last)

check :: State -> Result -> Result
check s@State {pairs, nonPairs, up, down, steps} next
    | any ((== 'M') . last) nonPairs && not (null pairs) = do
        tell $ D.fromList ["chip fried on " ++ show' s]
        return Nothing
    | null up && all null down = do
        tell $ D.fromList ["solution found in " ++ show steps ++ " steps"]
        return (Just s)
    | otherwise = next

go :: Dir -> [String] -> (State -> Result) -> State -> Result
go dir carry continue s@State {pairs, nonPairs, up, down, steps}
    | null carry || length carry > 2 = do
        tell $ D.fromList
            ["carry size" ++ (if null carry then " is below " else " exceeds ") ++ "limit"]
        return Nothing
    | otherwise = do
        let flr = filter (`notElem` carry) $ nonPairs ++ concat pairs
            (next, down', up') = case dir of
                Up -> (head up, flr : down, tail up)
                Down -> (head down, tail down, flr : up)
            (pairs', nonPairs') = group' $ next ++ carry
            newState = State
                { pairs = pairs', nonPairs = nonPairs'
                , down = down', up = up', steps = steps + 1}
         in continue newState

unpaired :: State -> Result
unpaired s@State {pairs, nonPairs, down, up, steps} = do
    tell $ D.fromList [show' s ++ " (in unpaired)"]
    check s $ if not (null nonPairs)
        then case canMove [] [] nonPairs of
            ([], []) -> do
                tell $ D.fromList ["no non pairs can move"]
                paired s
            (goingUp, goingDown) -> do
                s' <- foldM (f Up) Nothing $ filter ((<= 2) . length) $ tail $ subsequences goingUp
                s'' <- foldM (f Down) s' $ filter ((<= 2) . length) $ tail $ subsequences goingDown
                case s'' of
                    Nothing -> paired s
                    Just v -> return s''
        else do
            tell $ D.fromList ["no non pairs on floor"]
            paired s
    where
    f :: Dir -> Maybe State -> [String] -> Result
    f dir acc carry = do
        tell $ D.fromList [""]
        tell $ D.fromList ["trying " ++ show carry ++ " " ++ show dir ++ " from " ++ show (nonPairs ++ concat pairs)]
        r <- go dir carry unpaired s
        return $ min' acc r

    min' s1 s2 = s1 <> s2

    canMove up' down' [] = (up', down')
    canMove up' down' (p:xs)
        | noGeneratorsTill (findPair p) up = canMove (p : up') down' xs
        | noGeneratorsTill (findPair p) down = canMove up' (p : down') xs

    noGeneratorsTill p f =
        let flrs = takeTillIf (p `elem`) f
        -- works but I dont know how.  ¯\_(ツ)_/¯ $ if it aint broke, don't what?
         in not (all null flrs)
            || any (p `elem`) f && not (generator flrs)

paired :: State -> Result
paired s@State{pairs, down, up, steps} = do
    tell $ D.fromList [show' s ++ " (in paired)"]
    check s logic
    where
    logic | not (all null down) = do pairedDown s
          | length pairs >= 2 && not (null up) = twoPaired s
          | otherwise = case pairs of
                -- use others?
                (pair:xs) | generator up && moleculeNot up && all null down -> do
                    tell $ D.fromList [show pair ++ " can go up"]
                    go Up pair unpaired s
                _ -> do
                    tell $ D.fromList ["no pairs can go up"]
                    failed s

twoPaired :: State -> Result
twoPaired s@State{pairs, down, up, steps} = do
    let [p1, p2] = take 2 pairs
        elem1 = init (head p1)
        elem2 = init (head p2)
    tell $ D.fromList ["moving " ++ show [elem1, elem2] ++ " up"]
    -- this is a mess... the lengths we go to for generic code
    s' <- go Up [molecule elem1, molecule elem2] id' s
        >>= (go Down [molecule elem1] id' . fromJust)
        >>= (go Up [gen elem1, gen elem2] id' . fromJust)
        >>= (go Down [molecule elem2] id' . fromJust)
        >>= (go Up [molecule elem1, molecule elem2] id' . fromJust)
    case s' of
        Nothing -> unpaired s
        Just s'' -> do
            tell $ D.fromList [show' s'' ++ " (in two paired)"]
            unpaired s''
    where
    molecule el = el ++ "M"
    gen el = el ++ "G"
    id' = return . Just

pairedDown :: State -> Result
pairedDown s@State{pairs, nonPairs, down, up, steps} = do
    if null pairs
        then failed s
        else do
            tell $ D.fromList [""]
            tell $ D.fromList ["going down"]
            go Down (head pairs) unpaired s

failed :: State -> Result
failed s@State{pairs, nonPairs, down, up, steps} = do
    tell $ D.fromList ["(fail)"]
    return Nothing

-- pick unpaired objects on current floor
-- check if can be moved
    -- find pair
    -- no other generators between curr and other pair
-- add obj to elevator, check for another in
-- the same direction and go up

-- if generator above current floor
    -- pick paired objects on current floor
    -- check floor above
        -- does not contain another molecule
    -- add obj to elevator and go up

-- if there are generators below the current floor
    -- TODO: Need to choose which obj to carry below
    -- go down a floor and repeat above steps
-- else
    -- if there is more than one pair on the current floor
        -- move pair up one floor
    -- else
        -- move two pairs up
