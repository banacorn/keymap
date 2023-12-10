{-# LANGUAGE OverloadedStrings #-}

module Parser (parseAgdaInput, parseTex, parseTrie, parseExtension) where

import Control.Monad (void)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atto
import Data.Either (lefts, rights)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Types

--------------------------------------------------------------------------------
-- latin-ltx.el
--------------------------------------------------------------------------------

parseTex :: Parser [Translation]
parseTex = many1 $ parseTexTranslation <* skipGarbage

parseTexCode :: Parser Text
parseTexCode = do
  char '\"'
  result <-
    choice
      [ do
          string "\\\\"
          ignoreEscaped,
        ignoreEscaped
      ]
  char '\"'
  return $ removeSurroundingMoneySign result
  where
    removeSurroundingMoneySign :: Text -> Text
    removeSurroundingMoneySign raw =
      if Text.head raw == '$' && Text.last raw == '$'
        then
          let trimmed = Text.init (Text.tail raw)
           in if Text.head trimmed == '\\'
                then Text.tail trimmed
                else trimmed
        else raw

parseTexGlyph :: Parser Text
parseTexGlyph = do
  glyph <- Atto.take 1
  if Text.unpack glyph == "\\"
    then Atto.take 1
    else return glyph

parseTexTranslation :: Parser Translation
parseTexTranslation = do
  char '('
  code <- Text.unpack <$> parseTexCode
  skipSpaces
  char '?'
  glyph <- parseTexGlyph
  char ')'
  return $ Translation code [glyph]

--------------------------------------------------------------------------------
-- agda-input.el
--------------------------------------------------------------------------------

parseAgdaInput :: Parser [Translation]
parseAgdaInput = many1 $ parseTranslation <* skipGarbage

parseTranslation :: Parser Translation
parseTranslation = do
  char '('
  skipSpaces
  code <- fmap Text.unpack parseCode
  skipSpaces
  char '.'
  skipSpaces
  many' (char ',')
  char '('
  glyphs <- parseGlyphs
  char ')'
  char ')'
  return $ Translation code glyphs

parseCode :: Parser Text
parseCode = do
  char '\"'
  result <- ignoreEscaped
  char '\"'
  return result

parseGlyphs :: Parser [Text]
parseGlyphs = do
  skipSpaces
  concat
    <$> sepBy
      ( choice
          [ do
              char '\"'
              result <- ignoreEscaped
              char '\"'
              return (Text.words result >>= Text.group),
            do
              string "agda-input-to-string-list"
              return []
          ]
      )
      skipSpaces

--------------------------------------------------------------------------------
-- Trie
--------------------------------------------------------------------------------

parseTrie :: Parser Trie
parseTrie = do
  string "export default " <?> "header: export default "
  trie <- parseObject <?> "trie"
  choice [string ";\n", string ";"] <?> "closing semicolon"
  return trie

-- where
parseCandidates :: Parser [Text]
parseCandidates = do
  string "[" <?> "opening parenthese of candidates"
  candidates <- parseCode `sepBy` char ',' <?> "candidates"
  string "]" <?> "closing parenthese of candidates"
  return candidates

parsePair :: Parser (Either [Text] (Text, Trie))
parsePair = do
  key <- parseCode
  string ":" <?> "semicolon of pair"
  if key == ">>"
    then Left <$> parseCandidates
    else do
      trie <- parseObject
      return $ Right (key, trie)

parseObject :: Parser Trie
parseObject = do
  string "{" <?> "opening brace of pairs"
  pairs <- parsePair `sepBy1` char ',' <?> "pairs"
  string "}" <?> "closing brace of pairs"

  return $
    Node
      (HashMap.fromList (rights pairs))
      (concat $ lefts pairs)

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

parseExtension :: Parser [Translation]
parseExtension = Atto.many' parseTranslation
  where
    parseTranslation :: Parser Translation
    parseTranslation = do
      code <- Text.unpack <$> takeTill (== ' ')
      skipSpace
      glyphs <- takeTill Atto.isEndOfLine
      satisfy isEndOfLine
      return $ Translation code (Text.groupBy (\_ _ -> False) glyphs)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

skipSpaces :: Parser ()
skipSpaces = skipWhile (== ' ')

skipComment :: Parser ()
skipComment = do
  choice [string ";;", string ";"]
  skipWhile (/= '\n')
  char '\n'
  return ()

skipGarbage :: Parser [()]
skipGarbage =
  many' $
    choice
      [ skipComment,
        void $ char '\n',
        void $ char '\t',
        void $ many1 (char ' ')
      ]

ignoreEscaped :: Parser Text
ignoreEscaped = do
  next <- peekChar
  case next of
    Nothing -> return ""
    Just peeked -> case peeked of
      '\"' -> return "" -- ends everything
      '\\' -> do
        -- escape the next
        char '\\'
        escaped <- Atto.take 1
        rest <- ignoreEscaped
        return $ escaped <> rest
      others -> do
        _ <- anyChar
        rest <- ignoreEscaped
        return $ Text.singleton others <> rest
