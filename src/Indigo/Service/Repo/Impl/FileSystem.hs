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
import System.FilePath
import Data.Maybe (fromMaybe, catMaybes, fromJust, isJust, mapMaybe)
import Control.Lens ((^.))
import Control.Monad (guard, forM_, when, unless)
import GHC.Generics
import Data.Aeson

withHandle :: FilePath -> (Repo.Handle -> IO a) -> IO a
withHandle path action = do
  let repo = newHandle path
  initialize path repo
  action repo

metaPath path name = path </> "." <> T.unpack name <> ".json"
filePath path meta = path </> (meta ^. file)

existsMetaFile path name = doesFileExist $ metaPath path name
saveMetaFile path meta = B.writeFile (metaPath path (meta ^. name)) (BL.toStrict . encode . toMetaDto $ meta)
loadMetaFile path name = fromMetaDto name . fromJust . decode . BL.fromStrict <$> B.readFile (metaPath path name)

saveFile path meta text = B.writeFile (path </> meta ^. file) (T.encodeUtf8 text)
loadFile path meta = T.decodeUtf8 <$> B.readFile (path </> meta ^. file)

newHandle :: FilePath -> Repo.Handle
newHandle path = Repo.Handle listDocs loadMeta loadDoc saveDoc deleteDoc
  where
    listDocs :: IO [DocName]
    listDocs = mapMaybe extractName <$> listDirectory path
      where
        extractName :: FilePath -> Maybe DocName
        extractName f =
          case (dropExtension f, takeExtension f) of
            ('.':name, ".json")
              | isValidDocName name -> Just $ T.pack name
            _ -> Nothing
    loadMeta :: T.Text -> IO (Maybe DocMeta)
    loadMeta name = do
      exists <- existsMetaFile path name
      if exists
        then Just <$> loadMetaFile path name
        else pure Nothing
    loadDoc :: T.Text -> IO (Maybe Doc)
    loadDoc name = do
      exists <- existsMetaFile path name
      if exists
        then Just <$> (loadMetaFile path name >>= load)
        else pure Nothing
      where
        load :: DocMeta -> IO Doc
        load meta@DocMeta {_type = DocTypePage} = DocPage <$> loadFile path meta <*> pure meta
        load meta = pure $ DocImage meta
    saveDoc :: Doc -> IO Doc
    saveDoc page@DocPage {} = do
      saveMetaFile path (page ^. meta)
      saveFile path (page ^. meta) (page ^. text)
      pure page
    deleteDoc :: T.Text -> IO ()
    deleteDoc name = do
      exists <- existsMetaFile path name
      when exists $ do
        meta <- loadMetaFile path name
        removeFile (filePath path meta)
        removeFile (metaPath path name)

---

jsonConfig = defaultOptions { fieldLabelModifier = fieldLabelModifier }
  where
    fieldLabelModifier ('d':'t':'o':str) = T.unpack . T.toLower . T.pack $ str

data MetaDto = MetaDto
  { dtoType :: T.Text
  , dtoFile :: FilePath
  , dtoTags :: [T.Text]
  } deriving (Generic, Eq, Show)

instance ToJSON MetaDto where toJSON = genericToJSON jsonConfig
instance FromJSON MetaDto where parseJSON = genericParseJSON jsonConfig

fromMetaDto name metaDto =
  DocMeta
    { _name = name
    , _file = dtoFile metaDto
    , _tags = dtoTags metaDto
    , _type = fromTypeDto $ dtoType metaDto
    }
  where
    fromTypeDto "page" = DocTypePage
    fromTypeDto "image" = DocTypeImage

toMetaDto meta =
  MetaDto
    { dtoType = toTypeDto $ _type meta
    , dtoTags = meta ^. tags
    , dtoFile = meta ^. file
    }
  where
    toTypeDto DocTypePage = "page"
    toTypeDto DocTypeImage = "image"

---

initialize :: FilePath -> Repo.Handle -> IO ()
initialize path repo = do
  putStrLn "Initializing document repository..."

  let scan f
          | isValidDocName name = Just (f, T.pack name, takeExtension f)
          | otherwise = Nothing
        where name = dropExtension f

      defaultMeta f name ".md" = Just $ DocMeta name f DocTypePage []
      defaultMeta f name ext | ext `elem` [".png", ".jpg"] = Just $ DocMeta name f DocTypeImage []
      defaultMeta _ _ _ = Nothing

  -- Find markdown files without metadata, and add metadata for them.
  names <- mapMaybe scan <$> listDirectory path
  forM_ names $ \(f, name, ext) -> do
    exists <- existsMetaFile path name
    unless exists $ do
      let meta = defaultMeta f name ext
      case meta of
        Just meta -> saveMetaFile path meta
        Nothing -> mempty
