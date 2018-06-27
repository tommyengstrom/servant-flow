module Servant.Flow
    ( module Servant.Flow
    , defaultOptions
    , CodeGenOptions (..)
    ) where

import           Control.Lens
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text             as T
import           Servant.Flow.CodeGen
import           Servant.Flow.FlowType
import           Servant.Foreign
import Control.Monad.Reader

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
                            . fmap (flip runReader opts . renderFunction)
                            $ getEndpoints apiProxy


