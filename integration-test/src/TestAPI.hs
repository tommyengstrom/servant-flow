module TestAPI where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time
import           GHC.Generics (Generic)
import           Servant

data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON)

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
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] BigAssRecord


data BigAssRecord = BAR
    { barFoo            :: Int
    , barBool           :: Bool
    , barTransformation :: Transformation
    , barTime           :: UTCTime
    --, barDay :: Day
    } deriving (Show, Generic, ToJSON)
