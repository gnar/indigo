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
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Control.Lens ((^.))
import Control.Monad (guard)
import Data.List (isSuffixOf)
import GHC.Generics
import Data.Aeson

jsonConfig = defaultOptions { fieldLabelModifier = fieldLabelModifier' }
  where
    fieldLabelModifier' ('d':'t':'o':str) = T.unpack . T.toLower . T.pack $ str

data MetaDto = MetaDto
  { dtoTags :: [T.Text]
  , dtoType :: T.Text
  } deriving (Generic, Eq, Show)

instance ToJSON MetaDto where toJSON = genericToJSON jsonConfig
instance FromJSON MetaDto where parseJSON = genericParseJSON jsonConfig

fromMetaDto name metaDto =
  DocMeta
    { _name = name
    , _tags = dtoTags metaDto
    , _type = fromTypeDto $ dtoType metaDto
    }
  where
    fromTypeDto "page" = DocPage
    fromTypeDto "file" = DocFile

toMetaDto meta =
  MetaDto
    { dtoType = toTypeDto $ _type meta
    , dtoTags = meta ^. tags
    }
  where
    toTypeDto DocPage = "page"
    toTypeDto DocFile = "file"

newHandle :: FilePath -> Repo.Handle
newHandle path = Repo.Handle {
  listDocs = listDocs' path,
  loadDoc = loadDoc' path,
  loadMeta = loadMeta' path,
  saveDoc = saveDoc' path,
  deleteDoc = deleteDoc' path
}

---

metaFilePath path name = path <> "/" <> T.unpack name <> ".json" :: FilePath
pageFilePath path name = path <> "/" <> T.unpack name <> ".md" :: FilePath

existsDoc :: FilePath -> DocName -> IO Bool
existsDoc path name = doesFileExist $ metaFilePath path name

listDocs' :: FilePath -> IO [DocName]
listDocs' path = do
    listing <- listDirectory path
    pure $ catMaybes $ extractName <$> listing
  where
    extractName :: FilePath -> Maybe T.Text
    extractName f =
      case ".json" `T.breakOn` T.pack f of
        (name, ".json") -> Just name
        _ -> Nothing

loadMeta' :: FilePath -> T.Text -> IO (Maybe DocMeta)
loadMeta' path name = do
  exists <- existsDoc path name
  if exists
    then do
      meta <- fromMetaDto name . fromJust . decode . BL.fromStrict <$> B.readFile metaFile
      pure $ Just meta
    else pure Nothing
  where
    metaFile = metaFilePath path name

loadDoc' :: FilePath -> T.Text -> IO (Maybe Document)
loadDoc' path name = do
  exists <- existsDoc path name
  if exists
    then do
      meta <- fromJust <$> loadMeta' path name
      text <- T.decodeUtf8 <$> B.readFile (pageFilePath path name)
      pure $ case _type meta of
        DocPage -> Just $ Page text meta
        DocFile -> Nothing
    else pure Nothing

saveDoc' :: FilePath -> Document -> IO Document
saveDoc' path doc = do
    B.writeFile textFile $ T.encodeUtf8 (doc ^. text)
    B.writeFile metaFile $ BL.toStrict $ encode $ toMetaDto $ doc ^. meta
    pure doc
  where
    textFile = pageFilePath path (doc ^. meta . name)
    metaFile = metaFilePath path (doc ^. meta . name)

deleteDoc' :: FilePath -> T.Text -> IO ()
deleteDoc' path name = do
    removeFile textFile
    removeFile metaFile
  where
    textFile = pageFilePath path name
    metaFile = metaFilePath path name

withHandle :: FilePath -> (Repo.Handle -> IO a) -> IO a
withHandle path action = action (newHandle path)
