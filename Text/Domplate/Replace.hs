{-# LANGUAGE OverloadedStrings #-}
module Text.Domplate.Replace (Template, parseTemplate, replace) where
import Prelude  hiding (lookup)
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Applicative hiding (empty)
import Data.Maybe (catMaybes)
import Text.HTML.TagSoup
import Text.Domplate.Context
import Data.Yaml
import Data.ByteString (ByteString)

-- | A domplate template.
newtype Template = Template [Tag T.Text]

-- | Parse an HTML5 string into a template.
parseTemplate :: ByteString -> Template
parseTemplate = Template . parseTags . decodeUtf8

data InternalKey = Weak [Key] | Strong [Key]

-- | Perform substitutions on the given template using the given context,
--   returning a 'ByteString'.
replace :: Template -> Context -> Either String ByteString
replace = genericReplace encodeUtf8

-- | Perform substitutions on the given template using the given context,
--   returning a 'T.Text'.
replaceText :: Template -> Context -> Either String T.Text
replaceText = genericReplace id

genericReplace :: (T.Text -> a) -> Template -> Context -> Either String a
genericReplace conv (Template template) context =
    conv . renderTagsOptions opts . reverse <$> replace' template context
  where
    opts = renderOptions {optEscape = id}
    replace' ts ctx =
        step [] ts
      where
        -- Substitute tags and attributes for a list of tags.
        step acc (TagOpen name attrs : tags) = do
          attrs' <- catMaybes <$> mapM replaceAttr attrs
          substTag acc (TagOpen name attrs' : tags)
        step acc t = do
          substTag acc t

        substTag acc (tag@(TagOpen _ attrs):tags)
          | Just key <- P.lookup "insert" attrs =
            handleInsert acc (stripAttr "insert" tag) (mkKey key) tags

          | Just key <- P.lookup "replace" attrs =
            handleReplace acc tag (mkKey key) tags

          | Just key <- P.lookup "forall" attrs =
            handleForall acc (stripAttr "forall" tag) (mkKey key) tags

          | Just key <- P.lookup "when" attrs =
            handleWhen acc (stripAttr "when" tag) (mkKey key) tags

          | Just key <- P.lookup "unless" attrs =
            handleUnless acc (stripAttr "unless" tag) (mkKey key) tags

        substTag acc (tag:tags) = step (tag : acc) tags
        substTag acc []         = return acc

        -- Substitute attributes when/unless/insert:id:attr
        replaceAttr a@(k, val) =
          case T.splitOn ":" k of
            ["when", key, attr] -> do
              v <- nestedLookup (Bool False) (mkKey key) ctx
              case truthOf v of
                Just True  -> return (Just (attr, val))
                Just False -> return Nothing
                _          -> typeError (mkKey key) "bool" (typeOf v)
            ["unless", key, attr] -> do
              v <- nestedLookup (Bool False) (mkKey key) ctx
              case truthOf v of
                Just True  -> return Nothing
                Just False -> return (Just (attr, val))
                _          -> typeError (mkKey key) "bool" (typeOf v)
            ["insert", key, attr] -> do
              v <- nestedLookup (String "") (mkKey key) ctx
              case stringOf v of
                Just s -> return (Just (attr, s))
                _      -> typeError (mkKey key) "string" (typeOf v)
            _ -> do
              return $ Just a

        mkKey k
          | T.head k == '?' = Weak $ T.splitOn "." $ T.tail k
          | otherwise       = Strong $ T.splitOn "." k

        stripAttr s (TagOpen t as) = TagOpen t [a | a <- as, fst a /= s]

        handleInsert acc tag@(TagOpen nm _) key tags = do
          v <- nestedLookup (String "") key ctx
          case (dropUntilClose nm tags, stringOf v) of
            (ts', Just val) -> step (TagClose nm:TagText val:tag:acc) ts'
            _               -> typeError key "string" (typeOf v)

        handleReplace acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (String "") key ctx
          case stringOf v of
            Just s -> step (TagText s : acc) (dropUntilClose name tags)
            _      -> typeError key "string" (typeOf v)

        handleWhen acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (Bool False) key ctx
          case truthOf v of
            Just True  -> step (tag : acc) tags
            Just False -> step acc (dropUntilClose name tags)
            _          -> typeError key "bool" (typeOf v)

        handleUnless acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (Bool False) key ctx
          case truthOf v of
            Just False -> step (tag : acc) tags
            Just True  -> step acc (dropUntilClose name tags)
            _          -> typeError key "bool" (typeOf v)

        handleForall acc tag@(TagOpen name _) key tags = do
          let t = tag:takeUntilClose name tags ++ [TagClose name]
              rest = dropUntilClose name tags
              k = case key of
                    Strong k -> k
                    Weak k   -> k
          v <- nestedLookup (Array V.empty) key ctx
          case v of
            Array l -> do
              outs <- mapM (forallIter t k (V.length l-1))
                           (zip [0..] (V.toList l))
              step (concat (reverse (outs)) ++ acc) rest
            _ -> do
              typeError key "array" (typeOf v)

        forallIter t k lastIx (ix, v) = do
          ctx' <- nestedAdd k v ctx
          ctx'' <- if ix == 0
                     then nestedAdd ["_first"] (Bool True) ctx'
                     else nestedAdd ["_first"] (Bool False) ctx'
          ctx''' <- if ix == lastIx
                      then nestedAdd ["_last"] (Bool True) ctx''
                      else nestedAdd ["_last"] (Bool False) ctx''
          replace' t ctx'''

truthOf :: Value -> Maybe Bool
truthOf (Bool b)  = Just b
truthOf (Array a) = Just $ V.null a
truthOf _         = Nothing

stringOf :: Value -> Maybe T.Text
stringOf (String s)   = Just s
stringOf (Bool True)  = Just "true"
stringOf (Bool False) = Just "false"
stringOf (Number n)   = Just $ T.pack $ show n
stringOf _            = Nothing

nestedAdd :: [Key] -> Value -> Context -> Either String Context
nestedAdd key val ctx = go key ctx
  where
    go [k] m = do
      return $ add k val m
    go (k:ks) m = do
      case lookup k m of
        Just (Object ctx') -> do
          Ctx ctx'' <- go ks (Ctx ctx')
          return $ add k (Object ctx'') m
        _ -> do
          Ctx ctx' <- go ks empty
          return $ add k (Object ctx') m
    go _ _ = do
      notFoundError (Strong key)

-- | Lookup a value in a nested context.
nestedLookup :: Value -> InternalKey -> Context -> Either String Value
nestedLookup def key = go key'
  where
    (key', notFound) = case key of
          Weak k   -> (k, return def)
          Strong k -> (k, notFoundError key)
    go [k] m =
      case lookup k m of
        Just v -> return v
        _      -> notFound
    go (k:ks) m =
      case lookup k m of
        Just (Object m') -> go ks (Ctx m')
        _                -> notFound
    go _ _ =
      notFound

-- | Take tags until a matching closing tag is found. Does not return the
--   closing tag.
takeUntilClose :: T.Text -> [Tag T.Text] -> [Tag T.Text]
takeUntilClose str = go 0
  where
    go n (tag:tags) =
      case tag of
        TagOpen name _
          | voidTag name -> tag:go n tags
          | otherwise    -> tag:go (n+1) tags
        TagClose name
          | name == str && n == 0 -> []
          | otherwise             -> tag:go (n-1) tags
        _                         -> tag:go n tags
    go _ tags =
      []

-- | Drop tags until a matching closing tag is found. Drops the closing tag.
dropUntilClose :: T.Text -> [Tag T.Text] -> [Tag T.Text]
dropUntilClose str tags
  | voidTag str = tags
  | otherwise   = go 0 tags
    where
      go n (tag:tags) =
        case tag of
          TagOpen name _
            | voidTag name -> go n tags
            | otherwise    -> go (n+1) tags
          TagClose name
            | name == str && n == 0 -> tags
            | otherwise             -> go (n-1) tags
          _                         -> go n tags
      go _ tags =
        tags

voidTag :: T.Text -> Bool
voidTag t = t `elem` [
    "area", "base", "br", "col", "command", "embed", "hr", "img", "input",
    "keygen", "link", "meta", "param", "source", "track", "wbr"
  ]

-- | A type mismatch has occurred.
typeError :: InternalKey -> String -> String -> Either String a
typeError k tTemplate tCtx =
  Left $ unwords [
      T.unpack k', "was used as a", tTemplate, "by template,",
      "but declared", tCtx, "by context!"
    ]
  where
    k' = case k of
           Weak k'   -> T.intercalate (T.singleton '.') k'
           Strong k' -> T.intercalate (T.singleton '.') k'

-- | A key was not found.
notFoundError :: InternalKey -> Either String a
notFoundError k = Left $ T.unpack k' ++ " was not found in the context!"
  where
    k' = case k of
           Weak k'   -> T.intercalate (T.singleton '.') k'
           Strong k' -> T.intercalate (T.singleton '.') k'
