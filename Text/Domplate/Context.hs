module Text.Domplate.Context where
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.ByteString (ByteString)
import Data.Yaml
import Data.Monoid

-- | A key to be read from the context.
type Key = T.Text

-- | An Unplate context. A simple mapping from keys to values.
newtype Context = Ctx (M.HashMap Key Value)
  deriving Show

instance Monoid Context where
  mempty = empty
  mappend (Ctx a) (Ctx b) = Ctx $ M.union a b
  mconcat = Ctx . M.unions . map (\(Ctx ctx) -> ctx)

typeOf :: Value -> String
typeOf (String _) = "text"
typeOf (Number _) = "number"
typeOf (Bool _)   = "boolean"
typeOf (Array _)  = "array"
typeOf (Object _) = "object"
typeOf (Null)     = "null"

-- | Create a context from a JSON object. Throws an error if the value is not
--   an object.
fromJSON :: Value -> Context
fromJSON (Object o) = Ctx o
fromJSON _          = error "Tried to make a context out of a non-object!"

-- | Create a context from a list mapping keys to values.
fromList :: [(Key, Value)] -> Context
fromList = Ctx . M.fromList

-- | Create a JSON object from a context.
toValue :: Context -> Value
toValue (Ctx ctx) = Object ctx

-- | Add a new value to the given context. If the value already exists, it is
--   overwritten.
add :: Key -> Value -> Context -> Context
add k v (Ctx ctx) = Ctx $ M.insert k v ctx

-- | Remove a value from the given context.
remove :: Key -> Context -> Context
remove k (Ctx ctx) = Ctx $ M.delete k ctx

-- | An empty context.
empty :: Context
empty = Ctx M.empty

-- | Look up a value in the top level context.
lookup :: Key -> Context -> Maybe Value
lookup key (Ctx m) = M.lookup key m

-- | Get the size of the context. Nested contexts count a single element,
--   regardless of their size.
size :: Context -> Int
size (Ctx m) = M.size m

-- | Parse a context from a YAML-formatted 'ByteString'.
parseContext :: ByteString -> Either String Context
parseContext bs = do
  val <- decodeEither bs
  case val of
    Object ctx -> return $ Ctx ctx
    _          -> Left "Decoded value was not an object!"

