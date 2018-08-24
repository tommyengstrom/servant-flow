module Servant.Flow


    ( -- ** Classes and basic types
      FlowType
    , FlowTyped (..)
    , genericFlowType
    , renderFlowType
    , FlowObjectKey

    -- ** FlowTypeInfo for primative types
    , primBoolean, primNumber, primString, primAny, primAnyObject, primVoid

    -- ** Names and functions for converting FlowTypeInfo
    , nameless, withName, named

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
import           Servant.Flow.CodeGen
import           Servant.Flow.Internal
import           Servant.Foreign


data LangFlow

instance FlowTyped a => HasForeignType LangFlow FlowTypeInfo a where
    typeFor _ _ = flowTypeInfo

getEndpoints :: ( HasForeign LangFlow FlowTypeInfo api
                , GenerateList FlowTypeInfo (Foreign FlowTypeInfo api))
               => Proxy api -> [Req FlowTypeInfo]
getEndpoints = listFromAPI (Proxy @LangFlow) (Proxy @FlowTypeInfo)

generateFlowClient :: ( HasForeign LangFlow FlowTypeInfo api
                      , GenerateList FlowTypeInfo (Foreign FlowTypeInfo api))
                   => Proxy api -> CodeGenOptions -> Text
generateFlowClient apiProxy opts
    = execCodeGen opts
    . renderFullClientWithDefs
    $ getEndpoints apiProxy

generateClientFunction :: CodeGenOptions -> Text
generateClientFunction opts = execCodeGen opts renderClientFunction
