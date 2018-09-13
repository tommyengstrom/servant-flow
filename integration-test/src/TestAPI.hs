module TestAPI where

import           Data.Aeson
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Servant
import           Servant.Flow


data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON, FromJSON, Flow)

instance FromHttpApiData Transformation where
    parseUrlPiece "ToUpper" = Right ToUpper
    parseUrlPiece "ToLower" = Right ToLower
    parseUrlPiece a         = Left $ "Not a valid transformation: " <> a

instance ToHttpApiData Transformation where
    toUrlPiece ToUpper = "ToUpper"
    toUrlPiece ToLower = "ToLower"


type API = "capture"    :> CaptureAPI
      :<|> "queryparam" :> QueryParamAPI
      :<|> "reqbody"    :> ReqBodyAPI

type CaptureAPI
    =    "int"   :> Capture "int" Int              :> Get '[JSON] Int
    :<|> "text"  :> Capture "text" Text            :> Get '[JSON] Text
    :<|> "bool"  :> Capture "bool" Bool            :> Get '[JSON] Bool
    :<|> "trans" :> Capture "trans" Transformation :> Get '[JSON] Transformation

type QueryParamAPI
    =    "int"   :> QueryParam  "value" Int            :> Get '[JSON] Int
    :<|> "texts" :> QueryParams "value" Text           :> Get '[JSON] Text
    :<|> "bool"  :> QueryFlag   "true"                 :> Get '[JSON] Bool
    :<|> "trans" :> QueryParam  "value" Transformation :> Get '[JSON] Transformation

type ReqBodyAPI
    =    "int"   :> ReqBody '[JSON] Int             :> Post '[JSON] Int
    :<|> "text"  :> ReqBody '[JSON] Text            :> Post '[JSON] Text
    :<|> "bool"  :> ReqBody '[JSON] Bool            :> Post '[JSON] Bool
    :<|> "trans" :> ReqBody '[JSON] Transformation  :> Post '[JSON] Transformation
    :<|> "bar"   :> ReqBody '[JSON] BigAssRecord    :> Post '[JSON] BigAssRecord
    :<|> "rr"    :> ReqBody '[JSON] RecursiveRecord :> Post '[JSON] RecursiveRecord

data BigAssRecord = BAR
    { barFoo            :: Int
    , barBool           :: Bool
    , barTransformation :: Transformation
    } deriving (Show, Generic, FromJSON, ToJSON, Flow)

data RecursiveRecord = RR
    { rrRec   :: Maybe RecursiveRecord
    , rrValue :: Text
    } deriving (Show, Generic, FromJSON, ToJSON, Flow)
