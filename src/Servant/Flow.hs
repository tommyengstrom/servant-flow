module Servant.Flow where

import           Control.Lens
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
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

writeFlowForAPI :: ( HasForeign LangFlow FlowType api
                   , GenerateList FlowType (Foreign FlowType api))
                => Proxy api -> FilePath -> IO ()
writeFlowForAPI apiProxy path = T.writeFile path $ generateFlowClient apiProxy


generateFlowClient :: ( HasForeign LangFlow FlowType api
                      , GenerateList FlowType (Foreign FlowType api))
                   => Proxy api -> Text
generateFlowClient apiProxy = T.intercalate "\n\n"
                            . fmap mkEndpoint
                            $ getEndpoints apiProxy


