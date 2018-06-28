module Servant.Flow.CodeGen where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import           Network.HTTP.Types    (Method)
import           Servant.Flow.FlowType
import           Servant.Foreign

type CodeGenerator = Reader CodeGenOptions
type Indent = Int

-- | Options for how to generate the API client
data CodeGenOptions = CodeGenOptions
    { cgRenderFunctionName :: FunctionName -> Text -- ^ Name of endpoint function
    , cgIndentSize         :: Int
    }

defaultOptions :: CodeGenOptions
defaultOptions = CodeGenOptions
    { cgRenderFunctionName = camelCase
    , cgIndentSize         = 4
    }

argsToObject :: [Arg FlowType] -> FlowType
argsToObject = Object . fmap (\(Arg (PathSegment n) t) -> (n, t))

renderArg :: Arg FlowType -> Text
renderArg (Arg (PathSegment name) argType) = name <> showFlowTypeInComment argType

getCaptureArgs :: Req a -> [Arg a]
getCaptureArgs req = catMaybes . fmap getArg $ req ^. reqUrl . path
    where
        getArg :: Segment a -> Maybe (Arg a)
        getArg (Segment (Static _ )) = Nothing
        getArg (Segment (Cap a))     = Just a

getQueryArgs :: Req a -> [Arg a]
getQueryArgs = fmap (view queryArgName) . view (reqUrl . queryStr)

renderClientFunction :: CodeGenOptions -> Text
renderClientFunction = runReader genClient
    where
        genClient = do
            indent1 <- getIndentation 1
            indent2 <- getIndentation 2
            pure $ T.unlines
                [ "function createClient(token/* : string */, baseURL/* : string */)"
                , indent1 <> "{"
                , indent2 <> "return axios.create(" <> opts <> ");"
                , indent1 <> "};"
                ]
        opts = "{headers: {Authorization: token}, baseURL: baseURL}"


-- | Render javascript client function
renderFunction :: Req FlowType -> CodeGenerator Text
renderFunction req = do
    opts <- ask
    renderedBody <- renderFunctionBody 1 req
    renderedArgs <- renderArgs 1 req
    indent <- getIndentation 1
    pure $ "function " <> cgRenderFunctionName opts (req ^. reqFuncName)
        <> "(" <> renderedArgs
        <> "\n" <> indent <> ")"
        <> maybe ": void" showFlowTypeInComment (req ^. reqReturnType) <> "\n"
        <> renderedBody
    where

        renderArgs :: Indent -> Req FlowType -> CodeGenerator Text
        renderArgs i req = do
            indent <- getIndentation i
            let renderedReq = T.intercalate ",\n"
                            $ (indent <> "client/* : any */")
                            : (fmap (mappend indent . renderArg) arg)
                renderedOpt = case (getQueryArgs req, req ^. reqBody) of
                    ([] , Nothing) -> ""
                    (opt, Nothing) -> ",\n" <> indent <> "params "
                                   <> showFlowTypeInComment
                                        (argsToObject $ fmap (over argType Nullable) opt)
                    ([] , Just b) -> ",\n" <> indent <> "data " <> showFlowTypeInComment b
                    (opt, Just b) -> ",\n" <> indent <> "data " <> showFlowTypeInComment b
                                  <> ",\n" <> indent <> "params "
                                  <> showFlowTypeInComment
                                       (argsToObject $ fmap (over argType Nullable) opt)
                arg = getCaptureArgs req
                opt = getQueryArgs req

            pure $ "\n"
                <> renderedReq
                <> renderedOpt

        renderFunctionBody :: Indent -> Req FlowType -> CodeGenerator Text
        renderFunctionBody i req = do
            indent1 <- getIndentation i
            indent2 <- getIndentation $ i + 1
            indent3 <- getIndentation $ i + 2
            indentLet <- (<> T.replicate 14 " ") <$> getIndentation i

            let opts :: Text
                opts = T.intercalate ",\n" . fmap (indent3 <>) $ catMaybes
                    [ Just $ "url: url"
                    , Just $ "method: '" <> axiosMethod <> "'"
                    , fmap (const "data: data") $ req ^. reqBody
                    , fmap (const "params: params") . listToMaybe $ getQueryArgs req
                    ]
                axiosMethod = T.toLower . decodeUtf8 $ req ^. reqMethod
                urlPieces   = fmap renderSegment
                            $ req ^. reqUrl . path

                renderSegment :: Segment a -> Text
                renderSegment (Segment (Static (PathSegment t)))      = "\"" <> t <> "\""
                renderSegment (Segment (Cap (Arg (PathSegment n) _))) =
                    "encodeURIComponent(" <> n <> ")"

            pure $ T.unlines
                [ indent1 <> "{"
                , indent2 <> "let url = [ "
                    <> T.intercalate ("\n" <> indentLet <> ", ") urlPieces
                    <> "\n" <> indentLet <> "].join('/')"
                , indent2 <> "return client({\n" <> opts
                , indent3 <> "})"
                , indent1 <> "};"
                ]

getIndentation :: Indent -> CodeGenerator Text
getIndentation steps = do
    size <- asks cgIndentSize
    pure $ T.replicate (steps * size) " "


