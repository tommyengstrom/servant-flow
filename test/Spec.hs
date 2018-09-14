import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Servant
import           Servant.Flow
import           Test.Hspec


data Transformation = ToUpper | ToLower
    deriving (Show, Generic, ToJSON, FromJSON, Flow)

type API = "changeCase"
        :> Capture "transformation" Transformation
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text
    :<|> "user" :> ReqBody '[JSON] Text :> Post '[JSON] Text


main :: IO ()
main = hspec $ do
    describe "Generate API client" $ do
        it "Outputs something" $ do
            putStrLn . T.unpack $ T.replicate 80 "-"
            putStrLn . T.unpack $ generateFlowClient (Proxy @API) defaultCodeGenOptions
