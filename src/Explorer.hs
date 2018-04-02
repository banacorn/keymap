{-# LANGUAGE OverloadedStrings #-}
module Explorer where
--
import              Generator (readAndParse, sanitizeGlyph, growTrie)
import              Types
import              Parser
-- import              Data.IntMap.Lazy (IntMap)
-- import qualified    Data.IntMap.Lazy as IntMap
import              Data.HashMap.Lazy (HashMap)
import qualified    Data.HashMap.Lazy as HashMap
-- import              Data.ByteString.Lazy (ByteString)
-- import qualified    Data.ByteString.Lazy as BS
import              Data.Text (Text)
import qualified    Data.Text as Text
import qualified    Data.Text.IO as Text
import              Data.Char (ord)
-- import              Data.Aeson
-- import              Data.Aeson.Types (Pair)
import              Data.Monoid ((<>))
import              Data.Attoparsec.Text

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
        printEntries entries = "[" <> Text.concat entries  <> "]"
        printCandidates candidates = "[" <> Text.concat candidates  <> "]"

react :: String -> Char -> String
react xs c
    | isBackspace c &&      null xs  = []
    | isBackspace c && not (null xs) = init xs
    | isNewline c = xs
    | otherwise  = xs ++ [c]
    where
        isBackspace c = ord c == 127
        isNewline c = ord c == 10

trav :: Trie -> String -> ([Text], [Text])
trav (Node entries candidates) []     = (HashMap.keys entries, candidates)
trav (Node entries candidates) (x:xs) = case HashMap.lookup (Text.pack [x]) entries of
    Nothing -> ([], candidates)
    Just val -> trav val xs

--
-- test :: Parser [Translation] -> String -> IO ()
-- test parser path = do
--     raw <- readFile $ "assets/" <> path <> ".el"
--     parseTest parser (Text.pack raw)
--     return ()
--
-- --------------------------------------------------------------------------------
-- -- Trie
-- --------------------------------------------------------------------------------
--
-- growTrie :: [Translation] -> Trie
-- growTrie = foldr insert emptyTrie
--     where
--         insert :: Translation -> Trie -> Trie
--         insert (Translation [] glyphs) (Node entries candidates) = Node entries (nub (glyphs ++ candidates))
--         insert (Translation (x:xs) glyphs) (Node entries candidates) = Node entries' candidates
--             where
--                 entries' :: HashMap Text Trie
--                 entries' = case HashMap.lookup (Text.singleton x) entries of
--                     Just trie -> HashMap.insert (Text.singleton x) (insert (Translation xs glyphs) trie) entries
--                     Nothing -> HashMap.insert (Text.singleton x) (insert (Translation xs glyphs) emptyTrie) entries
--
--         emptyTrie :: Trie
--         emptyTrie = Node HashMap.empty []
--
-- serialize :: ToJSON a => a -> ByteString
-- serialize trie = "export default " <> encode trie <> ";"
--
-- cardinality :: Trie -> Int
-- cardinality (Node entries candidates) = length candidates + sum (map cardinality (HashMap.elems entries))
--
-- elems :: Trie -> [Text]
-- elems (Node entries candidates) = nub $ concat (candidates : map elems (HashMap.elems entries))
--
-- --------------------------------------------------------------------------------
-- -- Trie
-- --------------------------------------------------------------------------------
--
-- fetchLegacyTrie :: IO Trie
-- fetchLegacyTrie = do
--     raw <- BS.init . BS.init . BS.drop 15 <$> (BS.readFile "assets/legacy.ts")
--     case (decode raw :: Maybe Trie) of
--         Nothing -> error "failed"
--         Just trie -> return trie
--
-- --------------------------------------------------------------------------------
-- -- Unicode => Input Sequence
-- --------------------------------------------------------------------------------
--
-- growLookupTable :: [Translation] -> LookupTable
-- growLookupTable xs = foldr insert IntMap.empty $ xs >>= toEntries
--     where
--         insert :: Entry -> LookupTable -> LookupTable
--         insert (cp, codes) old = IntMap.insertWith (++) cp codes old
--
--         toEntries :: Translation -> [Entry]
--         toEntries (Translation code glyphs) = map (\g -> (toCodePoint g, [code])) glyphs
--
--         toCodePoint :: Text -> Int
--         toCodePoint x = head $ map fromEnum (Text.unpack x)
--
-- toJSONLookupTable :: LookupTable -> Value
-- toJSONLookupTable = object . map fromEntry . IntMap.toList
--     where
--         fromEntry :: Entry -> Pair
--         fromEntry (cp, codes) = (Text.pack (show cp), toJSON codes)
