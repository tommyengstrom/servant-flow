module Servant.Flow.FlowType where

-- import qualified Data.Map    as M
import           Data.Functor.Foldable
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T

class FlowTyped a where
    flowType :: Proxy a -> FlowType

instance FlowTyped Int where
    flowType _ = Fix $ Prim Number

instance FlowTyped Bool where
    flowType _ = Fix $ Prim Boolean

instance FlowTyped Text where
    flowType _ = Fix $ Prim String

instance FlowTyped a => FlowTyped (Maybe a) where
    flowType _ = Fix . Nullable $ flowType (Proxy @a)

instance {-# Overlappable #-} FlowTyped a where
    flowType _ = Fix $ Prim Any

data PrimType
  = Boolean
  | Number
  | String
  | Any
  deriving (Show, Eq, Ord)

data FlowTypeF a
    = Prim PrimType
    | Object [(Text, a)]
    | ExactObject [(Text, a)]
    | Array a
    | Nullable a
    deriving (Show, Eq, Functor, Traversable, Foldable)

type FlowType = Fix FlowTypeF

showFlowTypeInComment :: FlowType -> Text
showFlowTypeInComment t = "/* " <> showFlowType t <> " */"


inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inSuperBrackets :: Text -> Text
inSuperBrackets t = "{| " <> t <> " |}"

showFlowType :: FlowType -> Text
showFlowType = cata showFlowTypeF

showFlowTypeF :: FlowTypeF Text -> Text
showFlowTypeF (Prim Boolean) = "boolean"
showFlowTypeF (Prim Number)  = "number"
showFlowTypeF (Prim String)  = "string"
showFlowTypeF (Prim Any)     = "any"
showFlowTypeF (Nullable t)   = "?" <> t
showFlowTypeF (Array a)      = a <> "[]"
showFlowTypeF (Object l)     = inBrackets $ T.intercalate ", " (fmap (\(n, t) -> n <> t) l)
showFlowTypeF (ExactObject l)= inSuperBrackets $ T.intercalate ", " (fmap (\(n, t) -> n <> t) l)

