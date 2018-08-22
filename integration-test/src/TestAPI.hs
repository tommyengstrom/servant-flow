module TestAPI where

import           Data.Aeson
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Servant
import           Servant.Flow


data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON, FlowTyped)

instance FromHttpApiData Transformation where
    parseUrlPiece "ToUpper" = Right ToUpper
    parseUrlPiece "ToLower" = Right ToLower
    parseUrlPiece a         = Left $ "Not a valid transformation: " <> a

instance ToHttpApiData Transformation where
    toUrlPiece ToUpper = "ToUpper"
    toUrlPiece ToLower = "ToLower"


type API = "capture"    :> CaptureAPI
      :<|> "queryparam" :> QueryParamAPI

type CaptureAPI
    =    Capture "int" Int              :> Get '[JSON] Int
    :<|> Capture "text" Text            :> Get '[JSON] Text
    :<|> Capture "trans" Transformation :> Get '[JSON] Transformation

type QueryParamAPI
    =    "int"   :> QueryParam "value" Int            :> Get '[JSON] Int
    :<|> "text"  :> QueryParam "value" Text           :> Get '[JSON] Text
    :<|> "trans" :> QueryParam "value" Transformation :> Get '[JSON] Transformation
-- Get '[JSON] Text
--      :<|> Capture "int" Int :> Get '[JSON] Int
--      :<|> "query" :> QueryParam '[JSON] Text :> Get '[JSON]
--        :> Capture "transformation" Transformation
--        :> QueryParam "maxChars" Int
--        :> QueryFlag "fromEnd"
--        :> Get '[JSON] Text
--    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] BigAssRecord


data BigAssRecord = BAR
    { barFoo            :: Int
    , barBool           :: Bool
    , barTransformation :: Transformation
    -- , barTime           :: UTCTime
    } deriving (Show, Generic, ToJSON, FlowTyped)
