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
import System.FilePath (takeExtension, dropExtension)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Control.Lens ((^.))
import Control.Monad (guard)
import GHC.Generics
import Data.Aeson

withHandle :: FilePath -> (Repo.Handle -> IO a) -> IO a
withHandle path action = action (newHandle path)

newHandle :: FilePath -> Repo.Handle
newHandle path = Repo.Handle listDocs loadMeta loadDoc saveDoc deleteDoc
  where
    metaFilePath name = path <> "/" <> T.unpack name <> ".json"
    pageFilePath name = path <> "/" <> T.unpack name <> ".md"

    existsDoc :: DocName -> IO Bool
    existsDoc name = doesFileExist $ metaFilePath name

    listDocs :: IO [DocName]
    listDocs = do
        listing <- listDirectory path
        pure $ catMaybes $ extractDocName <$> listing
      where
        extractDocName f
          | takeExtension f == ".json" = Just . T.pack $ dropExtension f
          | otherwise = Nothing

    loadMeta :: T.Text -> IO (Maybe DocMeta)
    loadMeta name = do
      exists <- existsDoc name
      if exists
        then do
          meta <- fromMetaDto name . fromJust . decode . BL.fromStrict <$> B.readFile metaFile
          pure $ Just meta
        else pure Nothing
      where
        metaFile = metaFilePath name

    loadDoc :: T.Text -> IO (Maybe Doc)
    loadDoc name = do
      exists <- existsDoc name
      if exists
        then do
          meta <- fromJust <$> loadMeta name
          text <- T.decodeUtf8 <$> B.readFile (pageFilePath name)
          pure $ case _type meta of
            DocTypePage -> Just $ DocPage text meta
            DocTypeFile -> Nothing
        else pure Nothing

    saveDoc :: Doc -> IO Doc
    saveDoc doc = do
        B.writeFile textFile $ T.encodeUtf8 (doc ^. text)
        B.writeFile metaFile $ BL.toStrict $ encode $ toMetaDto $ doc ^. meta
        pure doc
      where
        textFile = pageFilePath (doc ^. meta . name)
        metaFile = metaFilePath (doc ^. meta . name)

    deleteDoc :: T.Text -> IO ()
    deleteDoc name = do
        removeFile textFile
        removeFile metaFile
      where
        textFile = pageFilePath name
        metaFile = metaFilePath name

---

jsonConfig = defaultOptions { fieldLabelModifier = fieldLabelModifier }
  where
    fieldLabelModifier ('d':'t':'o':str) = T.unpack . T.toLower . T.pack $ str

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
    fromTypeDto "page" = DocTypePage
    fromTypeDto "file" = DocTypeFile

toMetaDto meta =
  MetaDto
    { dtoType = toTypeDto $ _type meta
    , dtoTags = meta ^. tags
    }
  where
    toTypeDto DocTypePage = "page"
    toTypeDto DocTypeFile = "file"
