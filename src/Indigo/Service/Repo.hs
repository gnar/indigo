module Indigo.Service.Repo
  ( Handle(..)
  , withHandle
  , loadTextFile
  , saveTextFile
  , loadJsonFile
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Attoparsec.Text
import Data.Functor ((<&>))
import Data.Bool (bool)
import Data.Maybe (mapMaybe)
import System.FilePath (takeFileName, dropExtension, takeExtension, dropTrailingPathSeparator, (</>))
import System.Directory (listDirectory, doesFileExist, removeFile)

type Name = T.Text

data Handle = Handle
  { listFiles :: IO [(Name, T.Text)]
  , loadFile :: Name -> T.Text -> IO (Maybe B.ByteString)
  , saveFile :: Name -> T.Text -> B.ByteString -> IO ()
  , deleteFile :: Name -> T.Text -> IO ()
  }

generatePath :: Name -> T.Text -> FilePath
generatePath name "_text.md" = T.unpack $ name <> ".md"
generatePath name file = T.unpack $ mconcat [ name, "$", file ]

parsePath :: FilePath -> Maybe (Name, T.Text)
parsePath f =
    case parseOnly parser base of
        (Left _) -> Nothing
        (Right (name, ""))
          | ".md" <- ext -> Just (name, "_text.md")
        (Right (name, file))
          | False <- T.null file -> Just (name, file <> ext)
        (Right _) -> Nothing
  where
    tmp = takeFileName f
    (base, ext) = (T.pack $ dropExtension tmp, T.pack $ takeExtension tmp)
    parser = ((,) <$> takeWhile1 (/= '$') <*> option "" (char '$' *> takeText))

newHandle :: FilePath -> Handle
newHandle docRoot' = Handle listFiles loadFile saveFile deleteFile
  where
    docRoot = dropTrailingPathSeparator docRoot'
    listFiles = listDirectory docRoot <&> mapMaybe parsePath
    loadFile name file | p <- docRoot </> generatePath name file = doesFileExist p >>= bool (pure Nothing) (Just <$> B.readFile p)
    saveFile name file | p <- docRoot </> generatePath name file = B.writeFile p
    deleteFile name file | p <- docRoot </> generatePath name file = removeFile p

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle docRoot action = action (newHandle docRoot)

loadTextFile :: Handle -> Name -> T.Text -> IO (Maybe T.Text)
loadTextFile repo name file = loadFile repo name file <&> fmap T.decodeUtf8

saveTextFile :: Handle -> Name -> T.Text -> T.Text -> IO ()
saveTextFile repo name file text = saveFile repo name file (T.encodeUtf8 text)

loadJsonFile :: (FromJSON a) => Handle -> Name -> T.Text -> IO (Maybe a)
loadJsonFile repo name file = loadFile repo name file <&> (>>= decodeStrict)
