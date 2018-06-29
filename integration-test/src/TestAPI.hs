module TestAPI where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Monoid

data Transformation = ToUpper | ToLower
    deriving (Show, Generic)

instance FromHttpApiData Transformation where
    parseUrlPiece "ToUpper" = Right ToUpper
    parseUrlPiece "ToLower" = Right ToLower
    parseUrlPiece a         = Left $ "Not a valid transformation: " <> a

instance ToHttpApiData Transformation where
    toUrlPiece ToUpper = "ToUpper"
    toUrlPiece ToLower = "ToLower"


type API = "changeCase"
        :> Capture "transformation" Transformation
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] Text


