module Indigo.Service.Repo.Impl.FileSystem (
  withHandle
) where

import qualified Indigo.Service.Repo as Repo
import Indigo.Page

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import Data.Maybe (fromMaybe, catMaybes)
import Data.Aeson (decode, encode)
import Control.Lens ((^.))
import Control.Monad (guard)
import Data.List (isSuffixOf)

newHandle :: FilePath -> Repo.Handle
newHandle path = Repo.Handle {
  pageIndex = pageIndex' path,
  loadPage = loadPage' path,
  updatePage = updatePage' path,
  deletePage = deletePage' path
}

pageMetaFile path name = path <> "/" <> T.unpack name <> ".json" :: FilePath
pageTextFile path name = path <> "/" <> T.unpack name <> ".md" :: FilePath

pageIndex' :: FilePath -> IO [T.Text]
pageIndex' path = do
    listing <- listDirectory path
    pure $ catMaybes $ extractName <$> listing
  where
    extractName :: FilePath -> Maybe T.Text
    extractName f =
      case ".json" `T.breakOn` T.pack f of
        (name, ".json") -> Just name
        _ -> Nothing

loadPage' :: FilePath -> T.Text -> IO (Maybe Page)
loadPage' path name = do
  exists <- doesFileExist textFile
  if exists
    then do
      text <- T.decodeUtf8 <$> B.readFile textFile
      meta <- fromMaybe (PageMeta []) . decode . BL.fromStrict <$> B.readFile metaFile
      pure $ Just $ Page name text meta
    else pure Nothing
  where
    textFile = pageTextFile path name
    metaFile = pageMetaFile path name

updatePage' :: FilePath -> Page -> IO Page
updatePage' path page = do
    B.writeFile textFile $ T.encodeUtf8 (page ^. text)
    B.writeFile metaFile $ BL.toStrict $ encode (page ^. meta)
    pure page
  where
    textFile = pageTextFile path (page ^. name)
    metaFile = pageMetaFile path (page ^. name)

deletePage' :: FilePath -> T.Text -> IO ()
deletePage' path name = do
    removeFile textFile
    removeFile metaFile
  where
    textFile = pageTextFile path name
    metaFile = pageMetaFile path name

withHandle :: FilePath -> (Repo.Handle -> IO a) -> IO a
withHandle path action = action (newHandle path)
