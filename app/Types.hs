{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Monoid
import GHC.Generics

data Keymap = Keymap String [Text]

instance Show Keymap where
    show (Keymap folge glyphs) = "Keymap " ++ folge ++ " ["
        ++ Text.unpack (Text.intercalate (Text.singleton ' ') glyphs) ++ "]"

data Trie = Node (HashMap Text Trie) [Text]
      deriving (Show, Generic)

instance ToJSON Trie where
    toEncoding (Node sub candidates) = pairs (glyphs <> entries)
        where
            glyphs = case null candidates of
                True -> mempty
                False -> ">>" .= candidates
            entries = mconcat $ map (uncurry (.=)) (HashMap.toList sub)

instance FromJSON Trie where
    parseJSON (Object obj) = Node
        <$> parseEntries
        <*> candidates
        where
            candidates = case HashMap.lookup ">>" obj of
                Nothing -> return []
                Just array -> parseJSON array

            parseEntries :: Parser (HashMap Text Trie)
            parseEntries = sequence $ HashMap.map parseJSON (HashMap.delete ">>" obj)
