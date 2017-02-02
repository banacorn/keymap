{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Parser
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Data.Monoid ((<>))
import Data.Attoparsec.Text


main :: IO ()
main = do
    -- old <- legacyTrie
    -- new <- currentTrie
    --
    -- let o = Set.fromList $ elems old
    -- let n = Set.fromList $ elems new
    -- mapM_ (putStrLn . Text.unpack) (Set.difference o n)


    raw <- readFile "assets/extracted.el"
    case parseRaw raw of
        Just keymaps -> BS.writeFile "assets/result-mempty.ts" (serialize $ growTrie keymaps)
        Nothing -> print "parse error"

growTrie :: [Keymap] -> Trie
growTrie = foldr insert emptyTrie

insert :: Keymap -> Trie -> Trie
insert (Keymap [] glyphs) (Node sub candidates) = Node sub (candidates ++ glyphs)
insert (Keymap (x:xs) glyphs) (Node sub candidates) = Node sub' candidates
    where
        sub' :: HashMap Text Trie
        sub' = case HashMap.lookup (Text.singleton x) sub of
            Just trie -> HashMap.insert (Text.singleton x) (insert (Keymap xs glyphs) trie) sub
            Nothing -> HashMap.insert (Text.singleton x) (insert (Keymap xs glyphs) emptyTrie) sub

emptyTrie :: Trie
emptyTrie = Node HashMap.empty []

serialize :: Trie -> ByteString
serialize trie = "export default " <> encode trie <> ";"

legacyTrie :: IO Trie
legacyTrie = do
    raw <- BS.init . BS.init . BS.drop 15 <$> (BS.readFile "assets/legacy.ts")
    case (decode raw :: Maybe Trie) of
        Nothing -> error "failed"
        Just trie -> return trie

currentTrie :: IO Trie
currentTrie = do
    raw <- BS.init . BS.drop 15 <$> (BS.readFile "assets/result.ts")
    case (decode raw :: Maybe Trie) of
        Nothing -> error "failed"
        Just trie -> return trie

cardinality :: Trie -> Int
cardinality (Node entries glyphs) = length glyphs + sum (map cardinality (HashMap.elems entries))

elems :: Trie -> [Text]
elems (Node entries glyphs) = glyphs ++ (HashMap.elems entries >>= elems)
