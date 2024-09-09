module Main where

main = do
    contents <- lines <$> readFile "input.txt"

    putStr "Part One: "
    print $ foldl (f decompress) 0 contents

    putStr "Part Two: "
    print $ foldl (f decompressV2) 0 contents

    where f fn a v = a + fn v

splitAtFirst x = fmap (drop 1) . break (x ==)

decompress = length . f []
    where
    f s ('(':xs) =
        let (marker, rest) = splitAtFirst ')' xs
         in case splitAtFirst 'x' marker of
            (_, []) -> f ('(' : s) xs
            (num, reps) ->
                let (chars, after) = splitAt (read num) rest
                    str = concat $ replicate (read reps) (reverse chars)
                 in f (str ++ s) after
    f s (x:xs) = f (x : s) xs
    f s [] = s

decompressV2 :: String -> Int
decompressV2 = f 0
    where
    f n [] = n
    f n ('(':xs) =
        let (marker, rest) = splitAtFirst ')' xs
         in case splitAtFirst 'x' marker of
            (_, []) -> f (n + 2 + length marker) rest
            (num, rep) -> do
                let (sub, xs') = splitAt (read num) rest
                    v = (* read rep) $ f 0 sub
                f (n + v) xs'
    f n (_:xs) = f (n + 1) xs
