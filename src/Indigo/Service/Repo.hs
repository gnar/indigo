module Indigo.Service.Repo 
  ( Handle(..)
  , withHandle
  , loadOrCreateDoc
  ) where

import Indigo.Page
import Indigo.Config (guessMimeType)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe, catMaybes, fromJust, isJust, mapMaybe, maybe)
import Control.Lens ((^.))
import Control.Monad (guard, forM_, when, unless, filterM)
import GHC.Generics hiding (Meta)
import Data.Aeson
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Bool (bool)

data Handle = Handle {
    listDocs :: IO [PageName]
  , loadMeta :: PageName -> IO (Maybe Meta)
  , loadDoc :: PageName -> IO (Maybe Page)
  , saveDoc :: Page -> IO Page
  , deleteDoc :: PageName -> IO ()
  , loadFile :: PageName -> T.Text -> IO (Maybe B.ByteString)
}

loadOrCreateDoc :: Handle -> PageName -> IO Page
loadOrCreateDoc repo name = loadDoc repo name >>= maybe (saveDoc repo $ newDoc name) pure

---

scanPath :: FilePath -> Maybe (PageName, PageFile)
scanPath f =
    case parseOnly parser base of
        (Left _) -> Nothing
        (Right (name, ""))
          | ".md" <- ext -> Just (name, "_text.md")
          | ".json" <- ext -> Just (name, "_meta.json")
        (Right (name, file))
          | False <- T.null file -> Just (name, file <> ext)
        (Right _) -> Nothing
  where
    tmp = takeFileName f
    (base, ext) = (T.pack $ dropExtension tmp, T.pack $ takeExtension tmp)
    parser = option '.' (char '.') *> ((,) <$> takeWhile1 (/= '$') <*> option "" (char '$' *> takeText))

scanDirectory :: FilePath -> IO [(PageName, PageFile)]
scanDirectory docRoot = mapMaybe scanPath <$> listDirectory docRoot

genPath :: PageName -> PageFile -> FilePath
genPath name "_text.md" = T.unpack $ name <> ".md"
genPath name "_meta.json" = T.unpack $ "." <> name <> ".json"
genPath name file = T.unpack $ name <> "$" <> file

---

loadDocFile :: FilePath -> PageName -> PageFile -> IO (Maybe B.ByteString)
loadDocFile docRoot name file | p <- docRoot </> genPath name file = doesFileExist p >>= bool (pure Nothing) (Just <$> B.readFile p)

saveDocFile :: FilePath -> PageName -> PageFile -> B.ByteString -> IO ()
saveDocFile docRoot name file = B.writeFile (docRoot </> genPath name file)

loadMetaFile :: FilePath -> PageName -> IO (Maybe Meta)
loadMetaFile docRoot name = loadDocFile docRoot name "_meta.json" <&> fmap (fromMetaDto name . fromJust . decodeStrict')

saveMetaFile :: FilePath -> PageName -> Meta -> IO ()
saveMetaFile docRoot name meta = saveDocFile docRoot name "_meta.json" (BL.toStrict (encode $ toMetaDto meta))

loadTextFile :: FilePath -> PageName -> IO (Maybe T.Text)
loadTextFile docRoot name = loadDocFile docRoot name "_text.md" <&> fmap T.decodeUtf8

saveTextFile :: FilePath -> PageName -> T.Text -> IO ()
saveTextFile docRoot name text = saveDocFile docRoot name "_text.md" (T.encodeUtf8 text)

---

newHandle :: FilePath -> Handle
newHandle docRoot' =
    Handle listDocs loadMeta loadDoc saveDoc deleteDoc loadFile
  where
    docRoot = dropTrailingPathSeparator docRoot'

    listDocs :: IO [PageName]
    listDocs = scanDirectory docRoot <&> (\ps -> [docName | (docName, docFile) <- ps, docFile == "_text.md"])

    loadMeta :: PageName -> IO (Maybe Meta)
    loadMeta = loadMetaFile docRoot

    loadDoc :: PageName -> IO (Maybe Page)
    loadDoc name = do
      meta <- loadMetaFile docRoot name
      text <- loadTextFile docRoot name
      pure $ Page <$> (meta <|> Just (Meta name [])) <*> text

    saveDoc :: Page -> IO Page
    saveDoc doc = do
      saveMetaFile docRoot (doc ^. meta . name) (doc ^. meta)
      saveTextFile docRoot (doc ^. meta . name) (doc ^. text)
      pure doc

    deleteDoc :: PageName -> IO ()
    deleteDoc name = undefined

    loadFile :: PageName -> PageFile -> IO (Maybe B.ByteString)
    loadFile = loadDocFile docRoot

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle docRoot action = action (newHandle docRoot)

---

jsonConfig = defaultOptions { fieldLabelModifier = fieldLabelModifier }
  where
    fieldLabelModifier ('d':'t':'o':str) = T.unpack . T.toLower . T.pack $ str

newtype MetaDto = MetaDto { dtoTags :: [T.Text] } deriving (Generic, Eq, Show)

instance ToJSON MetaDto where toJSON = genericToJSON jsonConfig
instance FromJSON MetaDto where parseJSON = genericParseJSON jsonConfig

fromMetaDto name metaDto = Meta name (dtoTags metaDto)
toMetaDto meta = MetaDto (meta ^. tags)
