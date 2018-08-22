module Main where

import           Data.Proxy
import qualified Data.Text.IO       as T
import           Servant.Flow
import           System.Environment (getArgs)
import           TestAPI

main :: IO ()
main = do
    args <- getArgs
    path <- case args of
        [p] -> pure p
        _   -> error "Must be called with output path"
    let clientCode = generateClientFunction defaultCodeGenOptions
                  <> "\n\n"
                  <> generateFlowClient (Proxy @API) defaultCodeGenOptions
    T.writeFile path clientCode
