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
              :<|> pure . mconcat
              :<|> pure
              :<|> pure . fromMaybe ToUpper

reqBodyServer :: Server ReqBodyAPI
reqBodyServer = pure :<|> pure :<|> pure :<|> pure :<|> pure :<|> pure


app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = run 8284 . simpleCors $ logStdoutDev app
