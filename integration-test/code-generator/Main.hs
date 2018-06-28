module Main where

import           Data.Proxy
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Servant.Flow
import           TestAPI
import Data.Monoid
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    path <- case args of
        [p] -> pure p
        a   -> error "Must be called with output path"
    let clientCode = renderClientFunction defaultOptions
                  <> "\n\n"
                  <> generateFlowClient (Proxy @API) defaultOptions
    T.writeFile path clientCode

