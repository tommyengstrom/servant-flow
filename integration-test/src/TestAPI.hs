module TestAPI where

import Servant.Flow
import Servant
import Data.Text (Text)
import GHC.Generics (Generic)


data Transformation = ToUpper | ToLower
    deriving (Show, Generic)

type API = "changeCase"
        :> Capture "transformation" Transformation
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] Text


