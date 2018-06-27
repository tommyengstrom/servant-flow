import Servant.Flow
import Servant
import Test.Hspec

API = "something" :> Capture "cap" Text :> QueryParam "qparam" Int :> Get '[JSON] Text


main :: IO ()
main = putStrLn "Test suite not yet implemented"
