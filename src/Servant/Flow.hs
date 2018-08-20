module Servant.Flow


    ( -- ** Classes and basic types
      FlowType
    , FlowTyped (..)
    , genericFlowType
    , renderFlowType
    , FlowObjectKey

    -- ** Names and functions for converting FlowTypeInfo
    , nameless, withName, named

    -- ** FlowTypeInfo for primative types
    , primBoolean, primNumber, primString, primAny, primAnyObject, primVoid

    -- ** Code generation
    , CodeGenOptions (..)
    , defaultCodeGenOptions
    , renderClientFunction
    , generateClientFunction
    , getEndpoints
    , generateFlowClient

    -- ** Aeson rexports
    , Options (..)
    , defaultOptions
    , SumEncoding (..)
    , defaultTaggedObject

    -- ** Servant HasForeign
    , LangFlow

) where

import           Data.Aeson            (Options (..), SumEncoding (..), defaultOptions,
                                        defaultTaggedObject)
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Servant.Flow.CodeGen
import           Servant.Flow.Internal
import           Servant.Foreign


data LangFlow

instance FlowTyped a => HasForeignType LangFlow FlowType a where
    typeFor _ _ = flowType

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
