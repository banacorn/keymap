{-# LANGUAGE OverloadedStrings #-}

module Explorer where

import Data.Attoparsec.Text
import Data.Char (ord)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Main (growTrie, readAndParse, sanitizeGlyph)
import Parser
import Types

main :: IO ()
main = do
  -- keymaps drawn from two different sources
  agdaInput <- readAndParse parseAgdaInput "agda-input"
  tex <- readAndParse parseTex "latin-ltx"

  let sanitaized = map sanitizeGlyph (agdaInput ++ tex)
  let trie = growTrie sanitaized

  loop trie ""

loop :: Trie -> String -> IO ()
loop trie str = do
  c <- getChar
  let result = react str c
  let (entries, candidates) = trav trie result
  Text.putStrLn $ printEntries entries <> printCandidates candidates
  putStr $ "\r" <> result
  loop trie result
  where
    printEntries entries = "[" <> Text.concat entries <> "]"
    printCandidates candidates = "[" <> Text.concat candidates <> "]"

react :: String -> Char -> String
react xs c
  | isBackspace c && null xs = []
  | isBackspace c && not (null xs) = init xs
  | isNewline c = xs
  | otherwise = xs ++ [c]
  where
    isBackspace c = ord c == 127
    isNewline c = ord c == 10

trav :: Trie -> String -> ([Text], [Text])
trav (Node entries candidates) [] = (HashMap.keys entries, candidates)
trav (Node entries candidates) (x : xs) = case HashMap.lookup (Text.pack [x]) entries of
  Nothing -> ([], candidates)
  Just val -> trav val xs
