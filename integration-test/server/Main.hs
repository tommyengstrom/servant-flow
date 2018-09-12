module Main where

import           Data.Maybe
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           TestAPI

server :: Server API
server = captureServer
    :<|> queryParamServer
    :<|> reqBodyServer

captureServer :: Server CaptureAPI
captureServer = pure :<|> pure :<|> pure :<|> pure

queryParamServer :: Server QueryParamAPI
queryParamServer = pure . fromMaybe 42
              :<|> pure . fromMaybe ""
              :<|> pure . fromMaybe False
              :<|> pure . fromMaybe ToUpper

reqBodyServer :: Server ReqBodyAPI
reqBodyServer = pure :<|> pure :<|> pure :<|> pure :<|> pure :<|> pure

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
