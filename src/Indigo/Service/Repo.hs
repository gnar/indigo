module Indigo.Service.Repo
  ( Handle(..)
  , withHandle
  , loadTextFile
  , saveTextFile
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor ((<&>))
import Data.Bool (bool)
import Data.Maybe (mapMaybe, fromJust)
import System.FilePath
import Control.Exception
import System.Directory (listDirectory, doesFileExist, removeFile, makeAbsolute)

data Handle = Handle
  { listFiles :: IO [FilePath]
  , loadFile :: FilePath -> IO (Maybe B.ByteString)
  , saveFile :: FilePath -> B.ByteString -> IO ()
  , deleteFile :: FilePath -> IO ()
  }

newHandle :: FilePath -> Handle
newHandle rootPath = Handle listFiles loadFile saveFile deleteFile
  where
    makePath path = rootPath </> makeRelative rootPath path
    listFiles = listDirectory rootPath
    loadFile file | path <- makePath file = doesFileExist path >>= bool (pure Nothing) (Just <$> B.readFile path)
    saveFile file | path <- makePath file = B.writeFile path
    deleteFile file | path <- makePath file = removeFile path

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle docRoot action = action (newHandle docRoot)

loadTextFile :: Handle -> FilePath -> IO (Maybe T.Text)
loadTextFile repo path = loadFile repo path <&> fmap T.decodeUtf8

saveTextFile :: Handle -> FilePath -> T.Text -> IO ()
saveTextFile repo path text = saveFile repo path (T.encodeUtf8 text)
