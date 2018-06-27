module Servant.Flow.CodeGen where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Servant.Flow.FlowType
import           Servant.Foreign

-- data FuncArg = FuncArg Text FlowType

renderArg :: FuncArg -> Text
renderArg = undefined

type FuncName = Text
type FuncArg = Text
type FuncBody = Text
type ReturnType = FlowType

renderFunction :: FuncName -> [FuncArg] -> FuncBody -> ReturnType -> Text
renderFunction name args body ret
    = "function " <> name <> "\n"
   <> "  (" <> T.intercalate ",\n  " (fmap renderArg args) <> ") : " <> showFlowType ret <> "\n"
   <> "  { return "
   <> "  };"


mkEndpoint :: Req FlowType -> Text
mkEndpoint req =
    "function " <> reqName <> "(" <> T.intercalate ", " params <> ") : " <> retType
        <> "\n  { return axios." <> method <> "(" <> rPath <>  " + \"?\" + " <> rQuery <> ")"
        <> "\n  }"

    where
        reqName :: Text
        reqName = camelCase $ req ^. reqFuncName

        method :: Text
        method = "get"

        params :: [Text]
        params = fmap renderParam pathParams
              <> if null queryParams
                     then []
                     else ["opts: {" <> T.intercalate ", " (fmap renderParam queryParams) <> "}"]

        baseUrl :: Text
        baseUrl = quote "https://hihenry.com/api/v2"
            -- Fixme: declare this as a constant and make it environment dependent

        renderParam :: (Text, FlowType) -> Text
        renderParam (pName, pType) =
            pName <> ": " <> T.replace "\n" " " (showFlowType pType)

        rPath :: Text
        rPath = T.intercalate " + \"/\" + "
              . (baseUrl :)
              . fmap renderSegment
              $ req ^. reqUrl . path

        rQuery :: Text
        rQuery = "Object.entries(opts).map(function(kv) {return kv[0] + '=' + kv[1]}).join('&')"

        retType :: Text
        retType = maybe "void" (T.replace "\n" " " . showFlowType) $ req ^. reqReturnType

        pathParams :: [(Text, FlowType)]
        pathParams = fmap (over _1 unPathSegment)
                   . catMaybes
                   . fmap getPathParam
                   $ req ^. reqUrl . path

        getPathParam :: Segment FlowType -> Maybe (PathSegment, FlowType)
        getPathParam (Segment Static {}) = Nothing
        getPathParam (Segment (Cap a))   = Just (a ^. argName, a ^. argType)

        renderSegment :: Segment FlowType -> Text
        renderSegment (Segment (Static s)) =
            quote $ unPathSegment s
        renderSegment (Segment (Cap a))    =
            "encodeURIComponent(" <> unPathSegment (a ^. argName) <> ")"


        queryParams :: [(Text, FlowType)]
        queryParams = (\r -> ( unPathSegment $ view (queryArgName . argName) r
                             , view (queryArgName . argType) r)) -- fixme is may be a list or flag!
                  <$> req ^. reqUrl . queryStr

toFlowList :: FlowType -> FlowType
toFlowList = id -- fixme: obviously not correct!

-- Turn "string" into "\"string\""
quote :: Text -> Text
quote = T.cons '"' . flip T.snoc '"'

-- | 'HasForeignType' maps Haskell types with types in the target
-- language of your backend. For example, let's say you're
-- implementing a backend to some language __X__, and you want
-- a Text representation of each input/output type mentioned in the API:
--
-- > -- First you need to create a dummy type to parametrize your
-- > -- instances.
-- > data LangX
-- >
-- > -- Otherwise you define instances for the types you need
-- > instance HasForeignType LangX Text Int where
-- >    typeFor _ _ _ = "intX"
-- >
-- > -- Or for example in case of lists
-- > instance HasForeignType LangX Text a => HasForeignType LangX Text [a] where
-- >    typeFor lang type _ = "listX of " <> typeFor lang ftype (Proxy :: Proxy a)
--
-- Finally to generate list of information about all the endpoints for
-- an API you create a function of a form:
--
-- > getEndpoints :: (HasForeign LangX Text api, GenerateList Text (Foreign Text api))
-- >              => Proxy api -> [Req Text]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy Text) api
--
-- > -- If language __X__ is dynamically typed then you can use
-- > -- a predefined NoTypes parameter with the NoContent output type:
--
-- > getEndpoints :: (HasForeign NoTypes NoContent api, GenerateList Text (Foreign NoContent api))
-- >              => Proxy api -> [Req NoContent]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) api
-- >
--

