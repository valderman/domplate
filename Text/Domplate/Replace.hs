{-# LANGUAGE OverloadedStrings #-}
module Text.Domplate.Replace (Template, parseTemplate, replace) where
import Prelude as P
import Control.Applicative hiding (empty)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.HTML.TagSoup
import Text.Domplate.Context
import Text.Domplate.ContextParser

-- | A domplate template.
newtype Template = Template [Tag T.Text]

-- | Parse an HTML5 string into a template.
parseTemplate :: T.Text -> Template
parseTemplate = Template . parseTags

data InternalKey = Weak [Key] | Strong [Key]

-- | Perform substitutions on the given template using the given context.
replace :: Template -> Context -> Either String T.Text
replace (Template template) context =
    renderTagsOptions opts . reverse <$> replace' template context
  where
    opts = renderOptions {optEscape = id}
    replace' ts ctx =
        step [] ts
      where
        step acc (TagOpen name attrs : tags) = do
          attrs' <- catMaybes <$> mapM replaceAttr attrs
          step2 acc (TagOpen name attrs' : tags)
        step acc t = do
          step2 acc t

        step2 acc (tag@(TagOpen "replace" attrs):tags)
          | Just key <- P.lookup "with" attrs =
            handleReplace acc tag (mkKey key) tags
        step2 acc (tag@(TagOpen _ attrs):tags)
          | Just key <- P.lookup "insert"  attrs =
            handleInsert acc (stripAttr "insert" tag) (mkKey key) tags
          | Just key <- P.lookup "forall"  attrs =
            handleForall acc (stripAttr "forall" tag) (mkKey key) tags
          | Just key <- P.lookup "when"    attrs =
            handleWhen acc (stripAttr "when" tag) (mkKey key) tags
          | Just key <- P.lookup "unless"  attrs =
            handleUnless acc (stripAttr "unless" tag) (mkKey key) tags
        step2 acc (tag:tags) = step (tag : acc) tags
        step2 acc []         = return acc

        -- Substitute attributes when/unless/insert:id:attr
        replaceAttr a@(k, val) =
          case T.splitOn ":" k of
            ["when", key, attr] -> do
              v <- nestedLookup (Bool False) (mkKey key) ctx
              case v of
                Bool True  -> return (Just (attr, val))
                Bool False -> return Nothing
                List []    -> return Nothing
                List _     -> return (Just (attr, val))
                _          -> typeError (mkKey key) "bool" (typeOf v)
            ["unless", key, attr] -> do
              v <- nestedLookup (Bool False) (mkKey key) ctx
              case v of
                Bool True  -> return Nothing
                Bool False -> return (Just (attr, val))
                List []    -> return (Just (attr, val))
                List _     -> return Nothing
                _          -> typeError (mkKey key) "bool" (typeOf v)
            ["insert", key, attr] -> do
              v <- nestedLookup (Text "") (mkKey key) ctx
              case v of
                Bool True  -> return (Just (attr, "true"))
                Bool False -> return (Just (attr, "false"))
                Text s     -> return (Just (attr, s))
                _          -> typeError (mkKey key) "bool" (typeOf v)
            _ -> do
              return $ Just a

        mkKey k
          | T.head k == '?' = Weak $ T.splitOn "." $ T.tail k
          | otherwise       = Strong $ T.splitOn "." k

        stripAttr s (TagOpen t as) = TagOpen t [a | a <- as, fst a /= s]

        handleInsert acc tag@(TagOpen nm _) key tags = do
          v <- nestedLookup (Text "") key ctx
          case (dropUntilClose nm tags, v) of
            (ts', Text val)   -> step (TagClose nm:TagText val:tag:acc) ts'
            (ts', Bool True)  -> step (TagClose nm:TagText "true":tag:acc) ts'
            (ts', Bool False) -> step (TagClose nm:TagText "false":tag:acc) ts'
            _                 -> typeError key "text" (typeOf v)

        handleReplace acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (Text "") key ctx
          case v of
            Text val   -> step (TagText val : acc) tags
            Bool True  -> step (TagText "true" : acc) tags
            Bool False -> step (TagText "false" : acc) tags
            _          -> typeError key "text" (typeOf v)

        handleWhen acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (Bool False) key ctx
          case v of
            Bool True  -> step (tag : acc) tags
            Bool False -> step acc (dropUntilClose name tags)
            List []    -> step acc (dropUntilClose name tags)
            List _     -> step (tag : acc) tags
            _          -> typeError key "bool" (typeOf v)

        handleUnless acc tag@(TagOpen name _) key tags = do
          v <- nestedLookup (Bool False) key ctx
          case v of
            Bool False -> step (tag : acc) tags
            Bool True  -> step acc (dropUntilClose name tags)
            List []    -> step (tag : acc) tags
            List _     -> step acc (dropUntilClose name tags)
            _          -> typeError key "bool" (typeOf v)

        handleForall acc tag@(TagOpen name _) key tags = do
          let t = takeUntilClose name tags
              rest = dropUntilClose name tags
              k = case key of
                    Strong k -> k
                    Weak k   -> k
          v <- nestedLookup (List []) key ctx
          case v of
            List l -> do
              outs <- mapM (forallIter t k (length l-1)) (zip [0..] l)
              step (TagClose name:concat (reverse (outs)) ++ tag:acc) rest
            _ -> do
              typeError key "list" (typeOf v)

        forallIter t k lastIx (ix, v) = do
          ctx' <- nestedAdd k v ctx
          ctx'' <- if ix == 0
                     then nestedAdd ["_first"] (Bool True) ctx'
                     else nestedAdd ["_first"] (Bool False) ctx'
          ctx''' <- if ix == lastIx
                      then nestedAdd ["_last"] (Bool True) ctx''
                      else nestedAdd ["_last"] (Bool False) ctx''
          replace' t ctx'''

nestedAdd :: [Key] -> Value -> Context -> Either String Context
nestedAdd key val ctx = go key ctx
  where
    go [k] (Ctx m) = do
      return $ Ctx $ M.insert k val m
    go (k:ks) (Ctx m) = do
      case M.lookup k m of
        Just (Map ctx') -> do
          ctx'' <- go ks ctx'
          return $ Ctx $ M.insert k (Map ctx'') m
        _ -> do
          ctx' <- go ks empty
          return $ Ctx $ M.insert k (Map ctx') m
    go _ _ = do
      notFoundError (Strong key)

-- | Lookup a value in a nested context.
nestedLookup :: Value -> InternalKey -> Context -> Either String Value
nestedLookup def key = go key'
  where
    (key', notFound) = case key of
          Weak k   -> (k, return def)
          Strong k -> (k, notFoundError key)
    go [k] (Ctx m) =
      case M.lookup k m of
        Just v -> return v
        _      -> notFound
    go (k:ks) (Ctx m) =
      case M.lookup k m of
        Just (Map m') -> go ks m'
        _             -> notFound
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
dropUntilClose str = go 0
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
    "replace", "area", "base", "br", "col", "command",
    "embed", "hr", "img", "input", "keygen", "link",
    "meta", "param", "source", "track", "wbr"
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
