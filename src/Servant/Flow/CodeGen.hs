module Servant.Flow.CodeGen where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.RWS     hiding (Any)
import           Data.Function         (on)
import           Data.List
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import           Servant.Flow.Internal
import           Servant.Foreign


type Indent = Int

type CodeGen = RWS CodeGenOptions Text Indent

runCodeGen' :: CodeGen a -> CodeGenOptions -> (a, Indent, Text)
runCodeGen' g opts = runRWS g opts 0

execCodeGen :: CodeGenOptions -> CodeGen a -> Text
execCodeGen opts g = view _3 $ runCodeGen' g opts

-- | Options for how to generate the API client
data CodeGenOptions = CodeGenOptions
    { cgRenderFunctionName :: FunctionName -> Text -- ^ Name of endpoint function
    , cgIndentSize         :: Int
    , cgActivateFlow       :: Bool
    }

defaultCodeGenOptions :: CodeGenOptions
defaultCodeGenOptions = CodeGenOptions
    { cgRenderFunctionName = camelCase
    , cgIndentSize         = 4
    , cgActivateFlow       = True
    }

-- | Print indented
indent :: Text -> CodeGen ()
indent t = do
    indentSize <- asks cgIndentSize
    indentLevel <- get
    tell $ T.replicate (indentSize * indentLevel) " " <> t

-- | Print indented line
line :: Text -> CodeGen ()
line t = do
    tell "\n"
    indent t

-- | Indent the block one more step
indented :: CodeGen a -> CodeGen a
indented g = do
    indentMore
    a <- g
    indentLess
    pure a

indentMore :: CodeGen ()
indentMore = modify (+1)

indentLess :: CodeGen ()
indentLess = modify (\i -> max (i -1) 0)


renderFlowTypeInComment :: Rendering -> FlowTypeInfo -> Text
renderFlowTypeInComment r ty = "/* : " <> renderType r ty <> " */"

renderArg :: Rendering -> Arg FlowTypeInfo -> Text
renderArg r (Arg (PathSegment name) t) = name <> " " <> renderFlowTypeInComment r t

renderFlowTypeOneLine :: Rendering -> FlowTypeInfo -> Text
renderFlowTypeOneLine r = T.replace "\n" " " . renderType r


renderArgNoComment :: Rendering -> Arg FlowTypeInfo -> Text
renderArgNoComment r (Arg (PathSegment name) t) = name <> " : " <> renderFlowTypeOneLine r t

getCaptureArgs :: Req a -> [Arg a]
getCaptureArgs req = catMaybes . fmap getArg $ req ^. reqUrl . path
    where
        getArg :: Segment a -> Maybe (Arg a)
        getArg (Segment (Static _ )) = Nothing
        getArg (Segment (Cap a))     = Just a

getQueryArgs :: Req a -> [Arg a]
getQueryArgs = fmap (view queryArgName) . view (reqUrl . queryStr)

parens :: CodeGen a -> CodeGen a
parens g = do
    tell "("
    indented $ do
        a <- g
        line ")"
        pure a

block :: CodeGen a -> CodeGen a
block g = do
    tell "{"
    indented $ do
        a <- g
        line "}"
        pure a


renderClientFunction :: CodeGen ()
renderClientFunction = do
    line "function createClient"
    parens $ do
        line "token/* : string */,"
        line "baseURL/* : string */"
    block $ do
        line "return axios.create"
        parens . block $ do
            line "headers: "
            block $ line "Authorization: token"
            tell ","
            line "baseURL: baseURL"
    line "module.exports.createClient = createClient"

getFuncName :: Req FlowTypeInfo -> CodeGen Text
getFuncName req = do
    f <- asks cgRenderFunctionName
    pure . f $ req ^. reqFuncName

renderEndpointFunction :: Rendering -> Req FlowTypeInfo -> CodeGen ()
renderEndpointFunction r req = do
    funName <- getFuncName req
    line $ "function " <> funName
    parens renderAllArgs
    tell . renderFlowTypeInComment r . fromMaybe primAny $ _reqReturnType req
    block renderBody
    line $ "module.exports." <> funName <> " = " <> funName
    where
        -- Captures and request body
        args :: [Arg FlowTypeInfo]
        args = (getCaptureArgs req)
            <> maybe [] (\t -> [Arg (PathSegment "data") t]) (req ^. reqBody)

        qParams :: [Arg FlowTypeInfo]
        qParams = getQueryArgs req

        renderAllArgs :: CodeGen ()
        renderAllArgs  = do
            line "client /* : any */,"
            renderArgs True args
            unless (null qParams) $ do
                unless (null args) $ tell ","
                line "opts /* : "
                block $ renderArgs False qParams
                tell " */"

        renderArgs :: Bool -> [Arg FlowTypeInfo] -> CodeGen ()
        renderArgs _ []     = pure ()
        renderArgs inComment (a:as) = do
            if inComment
                then line $ renderArg r a
                else line $ renderArgNoComment r a
            unless (null as) $ tell ","
            renderArgs inComment as

        renderBody :: CodeGen ()
        renderBody = do
            line "return client"
            parens . block $ do
                line "url: ["
                indented $ do
                    renderUrl $ req ^. reqUrl . path
                    line "].join('/'),"
                line $ "method: '" <> (T.toLower . decodeUtf8 $ req ^. reqMethod) <> "'"
                unless (null qParams) $ do
                    tell ","
                    line "params: opts"
                unless (null $ req ^. reqBody) $ do
                    tell ","
                    line "data: data"

        renderUrl :: [Segment FlowTypeInfo] -> CodeGen ()
        renderUrl []     = pure ()
        renderUrl (Segment (Cap (Arg (PathSegment n) _)):ss) = do
            line $ "encodeURIComponent(" <> n <> ")"
            unless (null ss) $ tell ","
            renderUrl ss
        renderUrl (Segment (Static (PathSegment s)):ss) = do
            line $ "'" <> s <> "'"
            unless (null ss) $ tell ","
            renderUrl ss

renderEndpoints :: Rendering -> [Req FlowTypeInfo] -> CodeGen ()
renderEndpoints r endpoints = forM_ endpoints $ \endpoint -> do
    renderEndpointFunction r endpoint
    tell "\n\n"

renderFullClient :: Rendering -> [Req FlowTypeInfo] -> CodeGen ()
renderFullClient r endpoints = do
    activateFlow <- asks cgActivateFlow
    when activateFlow $ tell "// @flow \n\n"
    forM_ endpoints $ \endpoint -> do
        renderEndpointFunction r endpoint
        tell "\n\n"

renderFullClientWithDefs :: [Req FlowTypeInfo] -> CodeGen ()
renderFullClientWithDefs endpoints = do
    activateFlow <- asks cgActivateFlow
    when activateFlow $ tell "// @flow \n\n"

    renderTypeDefs endpoints
    renderEndpoints Referenced endpoints

renderTypeDefs :: [Req FlowTypeInfo] -> CodeGen ()
renderTypeDefs endpoints =
    forM_ (nubBy ((==) `on` fst) $ endpoints >>= getAllTypes >>= getRefEnv)
        $ \(name, ty) -> tell $ renderTypeDef name ty <> "\n\n"

renderTypeDef :: Text -> FlowTypeRef -> Text
renderTypeDef tyName ty = "type " <> tyName <> " = " <> renderFlowTypeWithReferences ty


getAllTypes :: Req flowTy -> [flowTy]
getAllTypes r = catMaybes [_reqBody r, _reqReturnType r]
