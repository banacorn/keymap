{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Monoid
import GHC.Generics

--------------------------------------------------------------------------------
-- Translation
--------------------------------------------------------------------------------

data Translation = Translation
    String  -- code
    [Text]  -- glyphs

instance Show Translation where
    show (Translation code glyphs) = "Translation " ++ code ++ " ["
        ++ Text.unpack (Text.intercalate (Text.singleton ' ') glyphs) ++ "]"

--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

data Trie = Node
    (HashMap Text Trie) -- entries
    (HashSet Text)      -- candidates
    deriving (Show, Generic)

instance ToJSON Trie where
    toEncoding (Node entries candidates) = pairs (candidates' <> entries')
        where
            candidates' = case null candidates of
                True -> mempty
                False -> ">>" .= candidates
            entries' = mconcat $ map (uncurry (.=)) (HashMap.toList entries)

instance FromJSON Trie where
    parseJSON (Object obj) = Node
        <$> entries
        <*> candidates
        where
            candidates :: Parser (HashSet Text)
            candidates = case HashMap.lookup ">>" obj of
                Nothing -> return HashSet.empty
                Just array -> parseJSON array

            entries :: Parser (HashMap Text Trie)
            entries = sequence $ HashMap.map parseJSON (HashMap.delete ">>" obj)
