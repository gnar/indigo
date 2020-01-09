module Main where

import Options.Generic
import Indigo.Server
import Indigo.Environment
import System.FilePath
import Data.Ini.Config

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

newtype Arguments = Arguments{config :: Maybe FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass ParseRecord

parseConfigFile :: FilePath -> IO (Either String Environment)
parseConfigFile path = T.readFile path <&> flip parseIniFile parse
  where
    parse = section "indigo" $
              Environment <$> fieldOf "host" string
                          <*> fieldOf "port" number
                          <*> fieldOf "wikipath" string
                          <*> fieldOf "mainpage" string

main :: IO ()
main = do
  Arguments{config} <- getRecord "Indigo personal Wiki"
  parseConfigFile (fromMaybe "indigo.ini" config) >>= \case
    (Right env) -> do
      print env
      runServer env
    (Left err) -> putStrLn $ "Error parse config file: " ++ err

test :: IO ()
test = do
  let env = Environment { _envHost = "localhost"
                        , _envPort = 8080
                        , _envStore = "wiki"
                        , _envMainPage = "Hauptseite"
                        }
  runServer env
