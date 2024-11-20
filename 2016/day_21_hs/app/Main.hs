module Main where

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.List
import Data.Maybe

main :: IO ()
main = do
    steps <- map words . lines <$> readFile "input.txt"

    putStr "Part One: "
    putStrLn $ foldl' p1 "abcdefgh" steps

    putStr "Part Two: "
    putStrLn $ foldl' p2 "fbgdceah" (reverse steps)

p1 :: String -> [String] -> String
p1 pass ["swap", "position", x, "with", "position", y] =
    swapBoth (pass !! read x) (pass !! read y) pass

p1 pass ["swap", "letter", [x], "with", "letter", [y]] = swapBoth x y pass

p1 pass ["rotate", dir, x, _] = rotate pass dir (read x)

p1 pass ["rotate", "based", "on", "position", "of", "letter", [x]] =
    let idx = fromJust $ elemIndex x pass
     in rotate pass "right" $ if idx >= 4 then idx + 2 else idx + 1

p1 pass ["reverse", "positions", a, "through", b] =
    let a' = read a
        (pre, rest) = splitAt a' pass
        (mid, post) = splitAt (read b + 1 - a') rest
     in pre ++ reverse mid ++ post

p1 pass ["move", "position", x, "to", "position", y] =
    let x' = read x
     in insert' (read y, pass !! x') $ delete (pass !! x') pass

p1 pass ins = error $ "invalid: " ++ unwords ins


p2 :: String -> [String] -> String
p2 pass ["rotate", dir, x, _] = rotate pass (rev' dir) (read x)
    where rev' "left" = "right"
          rev' "right" = "left"

-- 0 + 1 = 1  `mod` 8 = 1
-- 1 + 2 = 3  `mod` 8 = 3
-- 2 + 3 = 5  `mod` 8 = 5
-- 3 + 4 = 7  `mod` 8 = 7
-- 4 + 6 = 10 `mod` 8 = 2
-- 5 + 7 = 12 `mod` 8 = 4
-- 6 + 8 = 14 `mod` 8 = 6
-- 7 + 9 = 16 `mod` 8 = 0
--
-- x = (y + n) `mod` len; n = if y > 4 then y + 2 else y + 1
p2 pass ["rotate", "based", "on", "position", "of", "letter", [x]] =
    let idx = fromJust $ elemIndex x pass
        len = length pass
        inc y = if y >= 4 then y + 2 else y + 1
        assocs' = do
                y <- [0..len - 1]
                let x = (y + inc y) `mod` len
                [(x, y)]
        prevIdx = fromJust (lookup idx assocs')
     in if idx == 0
            then rotate pass "right" 1
            else rotate pass "left" (idx - prevIdx)

p2 pass ["move", "position", x, "to", "position", y] =
    let y' = read y
        pass' = delete (pass !! y') pass
     in insert' (read x, pass !! y') pass'

p2 pass ins = p1 pass ins

swapBoth :: Eq a => a -> a -> [a] -> [a]
swapBoth _ _ [] = []
swapBoth x y (ch:str)
    | ch `elem` [x, y] = head (delete ch [x, y]) : swapBoth x y str
    | otherwise = ch : swapBoth x y str

type Direction = String
rotate :: String -> Direction -> Int -> String
rotate pass dir n = case dir of "right" -> iterate rotateR pass !! n
                                "left" -> iterate rotateL pass !! n
                                _ -> error "invalid direction"
    where
    rotateL :: String -> String
    rotateL (s:str) = str ++ [s]

    rotateR :: String -> String
    rotateR str = last str : init str

insert' :: (Int, a) -> [a] -> [a]
insert' (i, v) vs = take i vs ++ v : drop i vs
