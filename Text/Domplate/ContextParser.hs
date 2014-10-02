{-# LANGUAGE OverloadedStrings #-}
module Text.Domplate.ContextParser (ParseError, parseContext) where
import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Text.Domplate.Context

-- | Parse a context from a simple line-based format.
--   A single line record is defined as a key followed by a colon, zero or more
--   spaces, and a value spanning the rest of the line (excluding newline).
--   Any spaces between the colon and the value are stripped.
--
--       key: value
--
--   A multiline record is defined as a key followed by a colon and zero or
--   more spaces. Any subsequent lines with n > 1 leading spaces are included
--   in the value, including newlines but excluding n leading spaces.
--   The newline following the value's final line is NOT included.
--   Any line with fewer than n leading spaces terminates the value.
--   The following example will thus parse into
--   fromList [(key, Text "line 1\n  line 2\nline 3")].
--
--       key:
--         line 1
--           line 2
--         line 3
--
--   A list is defined as a key followed by a colon, followed by any
--   number of dashes on the same indentation level. Then follows an arbitrary
--   number of multiline values, separated by the same number of dashes on the
--   same indentation level as the first.
--   The following example will parse into
--   fromList [(key, List [Text "val1.1\nval1.2",Text "val2",Text "val3"])].
--
--       key:
--       ---
--         val1.1
--         val1.2
--       ---
--         val2
--       ---
--         val3
--
--   A nested context is defined as a key followed by a colon, followed by a
--   context indented at least 1 space. The following example will parse into
--   fromList [
--       (key, Map $ fromList [(a, Text "hello"),(b, Text "multi\nline")]),
--       (key2, Text "end")
--     ]
--
--       key:
--         a: hello
--         b:
--           multi
--           line
--       key2:
--         end
--
--   Values of "true" and "false" are considered to be booleans, all other
--   simple values are considered to be text.
--   Empty lines are ignored, except when they occur within a multiline value.
--   Compound expressions may be arbitrarily nested.
parseContext :: T.Text -> Either ParseError Context
parseContext str = runParser pTopLevelContext () "" str

pTopLevelContext :: Parser Context
pTopLevelContext = spaces *> pContext <* spaces <* eof

pContext :: Parser Context
pContext = fromList <$> many pKVPair

pKVPair :: Parser (Key, Value)
pKVPair = try $ do
  k <- pKey
  skipMany spaceChar <* char ':' <* skipMany spaceChar
  v <- pValue
  return (k, tryMkCtx v)

pList :: Parser Value
pList = try $ do
  newline
  delim <- many1 $ char '-'
  List . map tryMkCtx <$> pMultiVal `sepBy` string delim

tryMkCtx :: Value -> Value
tryMkCtx (Text t) | Right ctx <- parseContext t, size ctx > 0 = Map ctx
tryMkCtx val                                                  = val

pKey :: Parser Key
pKey = T.pack <$> many1 (alphaNum <|> char '_')

pValue :: Parser Value
pValue = choice [pList, pSimpleVal]

-- | A single line value is a single line text or boolean.
pSingleVal :: Parser Value
pSingleVal = try $ choice [
    try $ string "true" >> return (Bool True),
    try $ string "false" >> return (Bool False),
    try $ Text . T.pack <$> many (noneOf "\n")
  ] <* choice [many1 newline >> pure (), eof]

spaceChar :: Parser Char
spaceChar = char ' '

-- | A text or boolean.
pSimpleVal :: Parser Value
pSimpleVal = choice [pMultiVal, pSingleVal]

-- | Read a multiline text or boolean.
pMultiVal :: Parser Value
pMultiVal = try $ do
  newline
  nspc <- lookAhead $ do
    skipMany newline
    length <$> many1 spaceChar
  when (nspc < 1) $ fail "Multiline value not indented."
  ls <- many $ pLine nspc
  let val = T.stripEnd $ T.unlines ls
  case val of
    "true"  -> return $ Bool True
    "false" -> return $ Bool False
    _       -> return $ Text val

-- | Read a line with at least n leading spaces, then strip the spaces.
--   Trailing newline is required, but not included in output.
pLine :: Int -> Parser T.Text
pLine nspc = try $ do
  l <- many spaceChar
  -- Fail if the line is nonempty and less indented than the first.
  if (length l < nspc)
    then do
      newline
      ml <- optionMaybe $ pLine nspc
      case ml of
        Just l -> return $ T.append (T.singleton '\n') l
        _      -> return $ T.singleton '\n'
    else do
      s <- many $ noneOf "\n"
      choice [newline >> pure (), eof]
      return $ T.pack $ drop nspc l ++ s
