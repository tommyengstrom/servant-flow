module Servant.Flow
    ( module Servant.Flow
    , defaultOptions
    , CodeGenOptions (..)
    , renderClientFunction
    ) where

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text             as T
import           Servant.Flow.CodeGen
import           Servant.Flow.FlowType
import           Servant.Foreign

data LangFlow

instance HasForeignType LangFlow FlowType a where
    typeFor _ _ _ = Any

getEndpoints :: ( HasForeign LangFlow FlowType api
                , GenerateList FlowType (Foreign FlowType api))
               => Proxy api -> [Req FlowType]
getEndpoints = listFromAPI (Proxy @LangFlow) (Proxy @FlowType)

generateFlowClient :: ( HasForeign LangFlow FlowType api
                      , GenerateList FlowType (Foreign FlowType api))
                   => Proxy api -> CodeGenOptions -> Text
generateFlowClient apiProxy opts = T.intercalate "\n\n"
                            . fmap (flip runCodeGen opts . renderFun)
                            $ getEndpoints apiProxy

generateClientFunction :: CodeGenOptions -> Text
generateClientFunction = runCodeGen renderClientFunction
