module Servant.Flow.CodeGen where

import           Control.Lens          hiding (List)
import           Control.Monad.Reader
import           Control.Monad.RWS     hiding (Any, Sum)
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.Functor.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import           GHC.Generics
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
    { cgRenderFunctionName    :: FunctionName -> Text -- ^ Name of endpoint function
    , cgIndentSize            :: Int  -- ^ number of spaces to indent by
    , cgActivateFlow          :: Bool -- ^ Include annotation to activate type checking
    , cgIncludeClientFunction :: Bool -- ^ Include function to create the Axios client
    }

defaultCodeGenOptions :: CodeGenOptions
defaultCodeGenOptions = CodeGenOptions
    { cgRenderFunctionName    = camelCase
    , cgIndentSize            = 4
    , cgActivateFlow          = True
    , cgIncludeClientFunction = True
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

parenBlock :: CodeGen a -> CodeGen a
parenBlock = blockWith "(" ")"

braceBlock :: CodeGen a -> CodeGen a
braceBlock = blockWith "{" "}"

bananaBlock :: CodeGen a -> CodeGen a
bananaBlock = blockWith "{|" "|}"

blockWith :: Text -> Text -> CodeGen a -> CodeGen a
blockWith open close cg = do
    tell open
    indented $ cg <* line close

indentMore :: CodeGen ()
indentMore = modify (+1)

indentLess :: CodeGen ()
indentLess = modify $ max 0 . subtract 1


renderFlowTypeInComment :: Rendering -> FlowTypeInfo -> Text
renderFlowTypeInComment r ty = "/* : " <> renderType r ty <> " */"

renderFlowTypeOneLine :: Rendering -> FlowTypeInfo -> Text
renderFlowTypeOneLine r = T.replace "\n" " " . renderType r


renderArgNoComment :: Rendering -> Arg FlowTypeInfo -> Text
renderArgNoComment r (Arg (PathSegment name) t) =
    name <> " : " <> renderFlowTypeOneLine r t

renderArg :: Rendering -> Arg FlowTypeInfo -> Text
renderArg r (Arg (PathSegment name) t) = name <> " " <> renderFlowTypeInComment r t


getCaptureArgs :: Req a -> [Arg a]
getCaptureArgs req = mapMaybe getArg $ req ^. reqUrl . path
    where
        getArg :: Segment a -> Maybe (Arg a)
        getArg (Segment (Static _ )) = Nothing
        getArg (Segment (Cap a))     = Just a

getQueryArgs :: Req FlowTypeInfo -> [Arg FlowTypeInfo]
getQueryArgs = fmap _queryArgName . view (reqUrl . queryStr)

mkOptionsType :: Req FlowTypeInfo -> FlowTypeInfo
mkOptionsType r = Fix . L1 . Object . fmap mkOptsField $ view (reqUrl . queryStr) r
    where
        mkOptsField :: QueryArg FlowTypeInfo -> PropertyF FlowTypeInfo
        mkOptsField (QueryArg (Arg p ty) _argTy) = OptionalProperty (unPathSegment p) ty

renderClientFunction :: CodeGen ()
renderClientFunction = do
    line "const axios = require('axios')\n"
    line "function createClient"
    parenBlock $ do
        line "token/* : string */,"
        line "baseURL/* : string */"
    braceBlock $ do
        line "return axios.create"
        parenBlock . braceBlock $ do
            line "headers: "
            braceBlock $ line "Authorization: token"
            tell ","
            line "baseURL: baseURL"
    line "module.exports.createClient = createClient\n"

getFuncName :: Req FlowTypeInfo -> CodeGen Text
getFuncName req = do
    f <- asks cgRenderFunctionName
    pure . f $ req ^. reqFuncName

renderEndpointFunction :: Rendering -> Req FlowTypeInfo -> CodeGen ()
renderEndpointFunction r req = do
    funName <- getFuncName req
    line $ "function " <> funName
    parenBlock renderAllArgs
    renderReturnType
    braceBlock renderBody
    line $ "module.exports." <> funName <> " = " <> funName
    where
        -- Captures and request body
        args :: [Arg FlowTypeInfo]
        args = getCaptureArgs req <>
            maybe [] (\t -> [Arg (PathSegment "data") t]) (req ^. reqBody)

        -- Query parameters and query flags
        qParams :: [Arg FlowTypeInfo]
        qParams = getQueryArgs req

        renderReturnType :: CodeGen ()
        renderReturnType =
            let mkResponse t = Fix . L1 . Promise . Fix . L1
                             $ Object [Property "status" primString, Property "data" t]
             in tell . renderFlowTypeInComment r
                     . mkResponse
                     . fromMaybe (Fix . L1 $ Prim Void)
                     $ _reqReturnType req

        renderAllArgs :: CodeGen ()
        renderAllArgs = do
            line "client /* : any */,"
            renderArgs True args
            unless (null qParams) $ do
                unless (null args) $ tell ","
                line "opts /* : "
                genFlowType . toReferenced $ mkOptionsType req
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
            parenBlock . braceBlock $ do
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
                    line "data: JSON.stringify(data)"

        renderUrl :: [Segment FlowTypeInfo] -> CodeGen ()
        renderUrl []     = pure ()
        renderUrl (Segment (Cap (Arg (PathSegment n) ty)):ss) = do
            let param = if isString ty then n else n <> ".toString()"
            line $ "encodeURIComponent(" <> param <> ")"
            unless (null ss) $ tell ","
            renderUrl ss
        renderUrl (Segment (Static (PathSegment s)):ss) = do
            line $ "'" <> s <> "'"
            unless (null ss) $ tell ","
            renderUrl ss

        isString :: FlowTypeInfo -> Bool
        isString (Fix (L1 (Prim String))) = True
        isString _                        = False


renderEndpoints :: Rendering -> [Req FlowTypeInfo] -> CodeGen ()
renderEndpoints r endpoints = forM_ endpoints $ \endpoint -> do
    renderEndpointFunction r endpoint
    tell "\n\n"

renderFullClientWithDefs :: [Req FlowTypeInfo] -> CodeGen ()
renderFullClientWithDefs endpoints = do
    activateFlow <- asks cgActivateFlow
    when activateFlow $ tell "// @flow \n"
    includeClient <- asks cgIncludeClientFunction
    when includeClient renderClientFunction
    tell "\n"
    renderTypeDefs endpoints
    renderEndpoints Referenced endpoints

renderTypeDefs :: [Req FlowTypeInfo] -> CodeGen ()
renderTypeDefs endpoints = do
    tell "/*::\n\n"
    forM_ (sortOn fst . nubBy ((==) `on` fst) $ endpoints >>= getAllTypes >>= getEnv)
        $ \(name, ty) -> genTypeDef name ty *> tell "\n\n"
    tell "*/\n\n"

renderTypeDef :: Text -> FlowTypeRef -> Text
renderTypeDef tyName ty = "export type " <> tyName <> " = "
    <> renderFlowTypeWithReferences ty


getAllTypes :: Req flowTy -> [flowTy]
getAllTypes r = catMaybes (_reqBody r : _reqReturnType r : fromURL (_reqUrl r))
    <> fmap fromHeader (_reqHeaders r)
    where
        fromURL :: Url a -> [Maybe a]
        fromURL (Url segments str) =
            fmap fromSegment segments <> fmap (Just . fromArg . _queryArgName) str

        fromSegment :: Segment a -> Maybe a
        fromSegment (Segment (Static _)) = Nothing
        fromSegment (Segment (Cap arg))  = Just (fromArg arg)

        fromArg :: Arg a -> a
        fromArg (Arg _name ty) = ty

        fromHeader :: HeaderArg a -> a
        fromHeader = fromArg . _headerArg


------------------------------------------------------------------------------------------
--  Type CodeGen
------------------------------------------------------------------------------------------


genTypeDef :: Text -> FlowTypeRef -> CodeGen ()
genTypeDef tyName ty = do
    tell $ "export type " <> tyName <> " = "
    genFlowType ty


genFlowType :: FlowTypeRef -> CodeGen ()
genFlowType = cata $ \case
    L1 ty  -> genFlowTypeF ty
    R1 ref -> tell $ unRef ref

genFlowTypeF :: FlowTypeF (CodeGen ()) -> CodeGen ()
genFlowTypeF (Prim prim)     = tell $ renderPrimative prim
genFlowTypeF (Nullable cg)   = tell "?" *> genParens cg
genFlowTypeF (Array cg)      = genParens cg *> tell "[]"
genFlowTypeF (Sum l)         = sequence_ $ intersperse (tell " | ") l
genFlowTypeF (Literal lit)   = tell $ showLiteral lit
genFlowTypeF (Promise cg)    = tell "Promise<" *> cg *> tell ">"
genFlowTypeF (Object props)  = braceBlock . sequence_ $ props <&> \p ->
    genProperty p
genFlowTypeF (ExactObject l) = blockWith "{|" "|}" . sequence_ $
    l <&> \(name, cg) ->
        line (name <> " : ") *> cg *> tell ","

genParens :: CodeGen a -> CodeGen a
genParens cg = tell "(" *> cg <* tell ")"

genProperty :: PropertyF (CodeGen ()) -> CodeGen ()
genProperty (Property fieldName ty)    = line (fieldName <> ": ") *> ty *> tell ","
genProperty (OptionalProperty key ty)  = line (key <> "?: ") *> ty *> tell ","
genProperty (IndexerProperty keyTy ty) = line "[" *> keyTy *> tell "]: " *> ty *> tell ","
