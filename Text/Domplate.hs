-- | Simple templating using HTML5 as the template language. Templates are
--   specified by adding special attributes to tags. During substitution, these
--   attributes are stripped from the HTML. The following attributes are
--   recognized:
--
--     * @insert="identifier"@ - replace the tag's contents with the value
--       bound to @identifier@ in the substitution context.
--     * @when="identifier"@ - only render this tag if @identifier@ is set to
--       true in the substitution context.
--     * @unless="identifier"@ - the dual of @when@; only render this tag if
--       @identifier@ is set to false in the substitution context.
--     * @forall="identifier"@ - render this tag and its contents once for each
--       element in the list bound to @identifier@ in the substitution context.
--       The contents of the element may refer to the current iteration's value
--       of @identifier@ by that same name.
--
--   Additionally, the special @replace@ tag will be replaced by whatever is
--   bound to its @with@ attribute: @\<replace with="identifier"\>@.
--
--   Substitution can also be performed on the attributes of tags. The
--   following attribute substitutions are recognized:
--
--     * @when:identifier:attr="value"@ - only include @attr@ is @identifier@
--       is set to true in the substitution context.
--     * @unless:identifier:attr="value"@ - only include @attr@ is @identifier@
--       is set to false in the substitution context.
--     * @insert:identifier:attr="value"@ - overwrite the value of @attr@ with
--       whatever @identifier@ is bound to in the substitution context.
--
--   Contexts can be nested, in which case nested keys are separated by
--   periods, as in @parent.child.grandchild@. Keys may be prefixed with a
--   question mark, in which case they are considered to be "weak keys".
--   If a weak key does not exist in the context, it will be replaced by a
--   sensible default value instead of causing an error. The defaults for the
--   different value types are as follows:
--
--     * bool: false
--     * string: ""
--     * array: []
--     * object: {}
--
--   As numbers are treated just like strings, they have an empty string as
--   their default value as well.
--
--   In general, values used as text must be declared text by the context and
--   so on, but the following coercions are permitted:
--
--     * bool to string
--     * array to bool
--
--   Coercion of array to bool, with the empty list being considered false and
--   all other list considered true, is permitted to allow templates to take
--   special action in the case of an empty list.
--
--   Contexts may be constructed programatically using the provided
--   combinators, converted from JSON objects or lists of key-value pairs, or
--   parsed from a YAML-formatted string using 'parseContext'.
module Text.Domplate (
    Text, Monoid, Template, Context, Value (..), Key,
    parseTemplate, replace,
    add, remove, fromList, Text.Domplate.Context.lookup, empty, size, (<>),
    parseContext,
    compile
  ) where
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Text (Text)
import Data.Yaml (Value (..))
import Text.Domplate.Context
import Text.Domplate.Replace
import qualified Data.ByteString as BS (readFile, writeFile)

-- | Compile a template using a context parsed from a context file.
--   Throws an error if context parsing or substitution fails.
compile :: FilePath -- ^ Template file.
        -> FilePath -- ^ Context file.
        -> FilePath -- ^ Output file.
        -> IO ()
compile template context outfile = do
  t <- parseTemplate <$> BS.readFile template
  ec <- parseContext <$> BS.readFile context
  case fmapL show ec >>= replace t of
    Right s -> BS.writeFile outfile s
    Left e  -> error e

fmapL :: (a -> b) -> Either a c -> Either b c
fmapL f (Left x)  = Left (f x)
fmapL _ (Right x) = Right x
