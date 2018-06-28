module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import           Servant
import           TestAPI
import Network.Wai
import Network.Wai.Handler.Warp

server :: Server API
server = changeCase
    :<|> user

changeCase :: Transformation -> Maybe Int -> Bool -> Handler Text
changeCase _ _ _ = pure "oh yeah!"

user :: Text -> Handler Text
user _ = pure "like a glove!"


app :: Application
app = serve (Proxy @API) server

main :: IO ()
main = run 8080 app

