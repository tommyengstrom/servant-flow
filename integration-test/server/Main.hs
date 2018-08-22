module Main where

-- import           Data.Text                            (Text)
-- import qualified Data.Text                            as T
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           TestAPI
import Data.Maybe

server :: Server API
server = captureServer
    :<|> queryParamServer
    :<|> reqBodyServer

captureServer :: Server CaptureAPI
captureServer = pure :<|> pure :<|> pure

reqBodyServer :: Server ReqBodyAPI
reqBodyServer = pure :<|> pure :<|> pure


queryParamServer :: Server QueryParamAPI
queryParamServer = pure . fromMaybe 42
              :<|> pure . fromMaybe "def"
              :<|> pure . fromMaybe ToUpper
--     :<|> user
--
-- changeCase :: Transformation -> Maybe Int -> Bool -> Handler Text
-- changeCase a b c = pure . T.pack $ show (a, b, c)
--
-- user :: Text -> Handler BigAssRecord
-- user _ = pure $ BAR 1 False ToUpper


app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = run 8284 . simpleCors $ logStdoutDev app
