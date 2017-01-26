{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text

main :: IO ()
main = do
    raw <- readFile "assets/extracted.el"
    let results = parseOnly (parseKeymaps) (Text.pack raw)
    case results of
        Left err -> print err
        Right val -> do
            print (length val)
            print (last val)

data Keymap = Keymap Text [Text]

instance Show Keymap where
    show (Keymap folge glyphs) = "Keymap " ++ Text.unpack folge ++ " ["
        ++ Text.unpack (Text.intercalate (Text.singleton ' ') glyphs) ++ "]"

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

parseKeymap :: Parser Keymap
parseKeymap = do
    char '('
    skipSpaces
    folge <- parseFolge
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

parseKeymaps :: Parser [Keymap]
parseKeymaps = do
    parseKeymap `sepBy1` skipGarbage
