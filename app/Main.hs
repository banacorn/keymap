{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Parser
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (notElem)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Attoparsec.Text

exclusions :: [String]
exclusions = ["geq", "leq", "bullet", "qed", "par", "newline"]

main :: IO ()
main = do
    agdaInput <- readAndParse parseAgdaInput "agda-input"
    tex <- readAndParse parseTex "latin-ltx"


    let filtered = filter (\(Translation code _) -> notElem code exclusions) (agdaInput ++ tex)
    let trie = growTrie filtered
    BS.writeFile "assets/keymap.ts" (serialize trie)

readAndParse :: Parser [Translation] -> String -> IO [Translation]
readAndParse parser path = do
    raw <- readFile $ "assets/" <> path <> ".el"
    case parseOnly parser (Text.pack raw) of
        Left err -> do
            print err
            return []
        Right val ->
            return val

test :: Parser [Translation] -> String -> IO ()
test parser path = do
    raw <- readFile $ "assets/" <> path <> ".el"
    parseTest parser (Text.pack raw)
    return ()



--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

growTrie :: [Translation] -> Trie
growTrie = foldr insert emptyTrie
    where
        insert :: Translation -> Trie -> Trie
        insert (Translation [] glyphs) (Node entries candidates) = Node entries (foldr HashSet.insert candidates glyphs)
        insert (Translation (x:xs) glyphs) (Node entries candidates) = Node entries' candidates
            where
                entries' :: HashMap Text Trie
                entries' = case HashMap.lookup (Text.singleton x) entries of
                    Just trie -> HashMap.insert (Text.singleton x) (insert (Translation xs glyphs) trie) entries
                    Nothing -> HashMap.insert (Text.singleton x) (insert (Translation xs glyphs) emptyTrie) entries

        emptyTrie :: Trie
        emptyTrie = Node HashMap.empty HashSet.empty

serialize :: Trie -> ByteString
serialize trie = "export default " <> encode trie <> ";"

cardinality :: Trie -> Int
cardinality (Node entries candidates) = length candidates + sum (map cardinality (HashMap.elems entries))

elems :: Trie -> HashSet Text
elems (Node entries candidates) = HashSet.unions (candidates : map elems (HashMap.elems entries))

--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

fetchLegacyTrie :: IO Trie
fetchLegacyTrie = do
    raw <- BS.init . BS.init . BS.drop 15 <$> (BS.readFile "assets/legacy.ts")
    case (decode raw :: Maybe Trie) of
        Nothing -> error "failed"
        Just trie -> return trie

-- fetchLegacyTrie :: IO Trie
-- currentTrie = do
--     raw <- BS.init . BS.drop 15 <$> (BS.readFile "assets/result.ts")
--     case (decode raw :: Maybe Trie) of
--         Nothing -> error "failed"
--         Just trie -> return trie
