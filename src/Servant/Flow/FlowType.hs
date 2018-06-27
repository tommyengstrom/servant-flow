module Servant.Flow.FlowType where

import           Data.Text (Text)

data FlowType
    = Any
    deriving (Show, Eq, Ord)

showFlowType :: FlowType -> Text
showFlowType Any = "any"

