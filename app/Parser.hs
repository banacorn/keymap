{-# LANGUAGE OverloadedStrings #-}

module Parser (parseRaw, parseKeymaps) where

import Types

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Data.Either (lefts, rights)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

parseRaw :: String -> Maybe [Keymap]
parseRaw raw =
    case parseOnly (parsePair `sepBy1` skipGarbage) (Text.pack raw) of
        Left err -> Nothing
        Right val -> Just val


parseFolge :: Parser Text
parseFolge = do
    char '\"'
    result <- takeTill ((==) '\"')
    char '\"'
    return result

parseCorrespondence :: Parser [Text]
parseCorrespondence = do
    skipSpaces
    fmap concat $ sepBy (choice [
            parseGlyphs
        ,   do
                string "agda-input-to-string-list"
                return []

        ]) skipSpaces

parseGlyphs :: Parser [Text]
parseGlyphs = do
    char '\"'
    result <- takeTill ((==) '\"')
    char '\"'
    return (Text.words result >>= Text.group)

parsePair :: Parser Keymap
parsePair = do
    char '('
    skipSpaces
    folge <- fmap Text.unpack parseFolge
    skipSpaces
    char '.'
    skipSpaces
    many' (char ',')
    char '('
    glyphs <- parseCorrespondence
    char ')'
    char ')'
    return $ Keymap folge glyphs

skipSpaces :: Parser ()
skipSpaces = skipWhile (== ' ')

isSpace :: Char -> Bool
isSpace c = c == ' '

skipComment :: Parser ()
skipComment = do
    string ";;"
    skipWhile (\c -> c /= '\n')
    char '\n'
    return ()

skipGarbage :: Parser [()]
skipGarbage = many' $ choice [
        skipComment,
        void $ char '\n',
        void $ many1 (char ' ')
    ]

parseKeymaps :: Parser Trie
parseKeymaps = do
    string "export default " <?> "header: export default "
    trie <- parseTrie
    choice [string ";\n", string ";"] <?> "ending"
    return trie
    -- where
parseCandidates :: Parser [Text]
parseCandidates = do
    string "[" <?> "["
    candidates <- parseFolge `sepBy` char ','
    string "]" <?> "]"
    return candidates

parseKV :: Parser (Either [Text] (Text, Trie))
parseKV = do
    key <- parseFolge
    string ":" <?> ":"
    if key == ">>"
        then Left <$> parseCandidates
        else do
            trie <- parseTrie
            return $ Right (key, trie)

parseTrie :: Parser Trie
parseTrie = do
    string "{" <?> "{"
    pairs <- parseKV `sepBy1` char ','
    p <- peekChar
    string "}" <?> show p
    return $ Node (HashMap.fromList (rights pairs)) (concat $ lefts pairs)
    where

a :: Text
a = "export default {\">>\":[],\"L\":{\">>\":[],\"u\":{\">>\":[],\"b\":{\">>\":[\"⨆\"]}}}};"

b :: Text
-- b = "{\">>\":[\"!\",\"$\"]}"
b = "{\">>\":[],\"L\":{\">>\":[],\"u\":{\">>\":[],\"b\":{\">>\":[\"⨆\"]}}}}"

c :: Text
c = "[\"!\",\"$\"]"

d :: Text
d = "[]"

p = parseTest parseTrie
r = parseTest parseKeymaps

q :: IO ()
q = do
    raw <- readFile "assets/short.ts"
    parseTest parseKeymaps (Text.pack raw)
