module Servant.Flow.FlowType where

import qualified Data.Map    as M
import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as T

data FlowType
    = Any
    | Object [(Text, FlowType)]
    | Nullable FlowType
    deriving (Show, Eq, Ord)

showFlowType :: FlowType -> Text
showFlowType Any          = ": any"
showFlowType (Nullable t) = "?" <> showFlowType t
showFlowType (Object l)   = ": { "
                         <> T.intercalate ", " (fmap (\(n, t) -> n <> showFlowType t) l)
                         <> " }"

