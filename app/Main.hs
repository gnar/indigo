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

parseConfigFile :: T.Text -> Either String Environment
parseConfigFile text = parseIniFile text parse
  where
    parse =
       Environment <$> section "server" (fieldOf "host" string)
                   <*> section "server" (fieldOf "port" number)
                   <*> section "config" (fieldOf "wikipath" string)
                   <*> section "config" (fieldOf "mainpage" string)

main :: IO ()
main = do
  args <- getRecord "The Indigo Personal Wiki"
  config <- T.readFile $ fromMaybe "indigo.ini" (config args)

  case parseConfigFile config of
    (Left err) -> putStrLn $ "Error parsing config file: " ++ err
    (Right env) -> runServer env
