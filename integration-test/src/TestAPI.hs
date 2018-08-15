module TestAPI where

import           Data.Aeson
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Servant
import           Servant.Flow.FlowType


data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON)

instance FlowTyped Transformation where
    flowType _ = primString

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
    -- , barTime           :: UTCTime
    } deriving (Show, Generic, ToJSON, FlowTyped)
