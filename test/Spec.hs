
import Servant.Flow
import Servant
import Test.Hspec
import Data.Text (Text)
import Data.Proxy
import qualified Data.Text as T

type API = "toUpper"
        :> Capture "text" Text
        :> QueryParam "maxChars" Int
        :> QueryFlag "fromEnd"
        :> Get '[JSON] Text



main :: IO ()
main = hspec $ do
    describe "Generate API client" $ do
        it "Outputs something" $ do
            let c = generateFlowClient (Proxy @API)
            putStrLn "\n\n"
            putStrLn $ T.unpack c
            putStrLn "\n\n"
