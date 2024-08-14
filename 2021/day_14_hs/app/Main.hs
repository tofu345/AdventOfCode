module Main where

import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.Regex.Posix
import Control.Monad (foldM)
import Data.Maybe
import Data.List
import Data.Ord
import System.IO
import System.Directory
import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
    contents <- T.readFile "test.txt"
    let (polymer, pairs) = parse contents
    -- print polymer
    partOne pairs polymer 10
    return ()

type Steps = Int
type Pairs = Map.Map Text Text

parse :: Text -> (Text, Pairs)
parse contents = do
    let lines' = lines (T.unpack contents)
        pairs = Prelude.foldl
            (\acc v ->
                let [[_, from, to]] = v =~ "(.*) -> (.*)" :: [[String]]
                 in Map.insert (T.pack from) (T.pack to) acc)
            Map.empty
            $ drop 2 lines'
    (T.pack (head lines'), pairs)

partOne :: Pairs -> Text -> Steps -> IO ()
partOne pairs polymer steps = do
    (inputName, inputHandle) <- openTempFile "." "temp"
    (outputName, outputHandle) <- openTempFile "." "temp"
    T.hPutStr inputHandle polymer
    hClose inputHandle
    hClose outputHandle

    -- try strict IO and iteratee, also actually learn what that means 
    -- https://stackoverflow.com/a/2984489

    iterate' inputName outputName 10
    where
        iterate' :: FilePath -> FilePath -> Int -> IO ()
        iterate' inputName' outputName' count = do
            putStr "> " >> print count

            inputHandle' <- openFile inputName' ReadMode
            outputHandle' <- openFile outputName' WriteMode

            inputContents <- T.hGetContents inputHandle'
            -- print inputContents
            T.hPutStr outputHandle' $ T.take 1 inputContents
            recurse outputHandle' inputContents

            hClose inputHandle'
            hClose outputHandle'
            if count > 1
               then iterate' outputName' inputName' (count - 1)
               else do
                   -- room for improvement here.
                   outputHandle'' <- openFile outputName' ReadMode
                   contents <- T.hGetContents outputHandle''
                   ht <- H.new :: IO (HashTable Char Int)
                   calcQuantities ht contents

                   quantities <- H.toList ht
                   let quantities' = sort $ map snd quantities
                   print $ last quantities' - head quantities'

                   hClose outputHandle''
                   removeFile inputName'
                   removeFile outputName'

        recurse :: Handle -> Text -> IO ()
        recurse outputHandle contents = do
            let pair = T.take 2 contents
                c = fromJust $ Map.lookup pair pairs
            if T.length pair < 2
               then return ()
               else do
                    T.hPutStr outputHandle $ T.snoc c (T.last pair)
                    -- print pair
                    recurse outputHandle $ T.drop 1 contents

        calcQuantities :: HashTable Char Int -> Text -> IO ()
        calcQuantities ht contents = do
            case T.uncons contents of
                Just (ch, rest) -> do
                    res <- H.lookup ht ch
                    case res of
                        Nothing -> H.insert ht ch 1
                        Just v  -> H.insert ht ch (v+1)
                    calcQuantities ht rest
                Nothing -> return ()
