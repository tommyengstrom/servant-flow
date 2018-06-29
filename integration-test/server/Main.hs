module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import           Servant
import           TestAPI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors

server :: Server API
server = changeCase
    :<|> user

changeCase :: Transformation -> Maybe Int -> Bool -> Handler Text
changeCase a b c = pure . T.pack $ show (a, b, c)

user :: Text -> Handler Text
user _ = pure "like a glove!"


app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = run 8080 . simpleCors $ logStdoutDev app

