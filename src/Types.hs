{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types (Pair, Parser)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Monoid
import qualified Data.String as String
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

--------------------------------------------------------------------------------
-- Translation
--------------------------------------------------------------------------------

data Translation
  = Translation
      String -- code
      [Text] -- glyphs

instance Show Translation where
  show (Translation code glyphs) =
    "Translation "
      ++ code
      ++ " ["
      ++ Text.unpack (Text.intercalate (Text.singleton ' ') glyphs)
      ++ "]"

--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

data Trie
  = Node
      (HashMap Text Trie) -- entries
      [Text] -- candidates
  deriving (Show, Generic)

instance ToJSON Trie where
  toJSON (Node entries candidates) =
    let entries' = Aeson.KeyMap.fromHashMapText (fmap toJSON entries)
     in Object $
          if null candidates
            then entries'
            else Aeson.KeyMap.insert ">>" (toJSON candidates) entries'

instance FromJSON Trie where
  parseJSON (Object obj) =
    Node
      <$> entries
      <*> candidates
    where
      candidates :: Parser [Text]
      candidates = case Aeson.KeyMap.lookup ">>" obj of
        Nothing -> return []
        Just array -> parseJSON array

      entries :: Parser (HashMap Text Trie)
      entries = Aeson.KeyMap.toHashMapText <$> mapM parseJSON (Aeson.KeyMap.delete ">>" obj)

--------------------------------------------------------------------------------
-- Lousy lookup table from Unicode symbols (code points) to input sequences
--------------------------------------------------------------------------------

type LookupTable = IntMap [String]

type Entry = (Int, [String])
