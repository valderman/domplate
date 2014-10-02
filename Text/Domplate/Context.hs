module Text.Domplate.Context where
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid

-- | A key to be read from the context.
type Key = T.Text

-- | A typed domplate value. May be either a string or a boolean.
data Value
  = Text !T.Text
  | Bool !Bool
  | List ![Value]
  | Map  !Context
    deriving Show

-- | An Unplate context. A simple mapping from keys to values.
newtype Context = Ctx (M.Map Key Value)
  deriving Show

instance Monoid Context where
  mempty = empty
  mappend (Ctx a) (Ctx b) = Ctx $ M.union a b
  mconcat = Ctx . M.unions . map (\(Ctx ctx) -> ctx)

-- | Lookup a value in the topmost context.
lookup :: Key -> Context -> Maybe Value
lookup k (Ctx m) = M.lookup k m

typeOf :: Value -> String
typeOf (Text _) = "text"
typeOf (Bool _) = "boolean"
typeOf (List _) = "list"
typeOf (Map _)  = "map"

showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"

-- | Create a context from a list mapping keys to values.
fromList :: [(Key, Value)] -> Context
fromList = Ctx . M.fromList

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

-- | Get the size of the context. Nested contexts count a single element,
--   regardless of their size.
size :: Context -> Int
size (Ctx m) = M.size m
