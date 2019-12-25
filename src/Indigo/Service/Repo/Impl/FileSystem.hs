module Indigo.Service.Repo.Impl.FileSystem (
  withHandle
) where

import qualified Indigo.Service.Repo as Repo
import Indigo.Doc
import Indigo.Config (guessMimeType)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe, catMaybes, fromJust, isJust, mapMaybe)
import Control.Lens ((^.))
import Control.Monad (guard, forM_, when, unless, filterM)
import GHC.Generics
import Data.Aeson
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Bool (bool)

---

scanPath :: FilePath -> Maybe (DocName, DocFile)
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

scanDirectory :: FilePath -> IO [(DocName, DocFile)]
scanDirectory docRoot = mapMaybe scanPath <$> listDirectory docRoot

genPath :: DocName -> DocFile -> FilePath
genPath name "_text.md" = T.unpack $ name <> ".md"
genPath name "_meta.json" = T.unpack $ "." <> name <> ".json"
genPath name file = T.unpack $ name <> "$" <> file

---

loadDocFile :: FilePath -> DocName -> DocFile -> IO (Maybe B.ByteString)
loadDocFile docRoot name file | p <- docRoot </> genPath name file = doesFileExist p >>= bool (pure Nothing) (Just <$> B.readFile p)

saveDocFile :: FilePath -> DocName -> DocFile -> B.ByteString -> IO ()
saveDocFile docRoot name file = B.writeFile (docRoot </> genPath name file)

loadMetaFile :: FilePath -> DocName -> IO (Maybe DocMeta)
loadMetaFile docRoot name = loadDocFile docRoot name "_meta.json" <&> fmap (fromMetaDto name . fromJust . decodeStrict')

saveMetaFile :: FilePath -> DocName -> DocMeta -> IO ()
saveMetaFile docRoot name meta = saveDocFile docRoot name "_meta.json" (BL.toStrict (encode $ toMetaDto meta))

loadTextFile :: FilePath -> DocName -> IO (Maybe T.Text)
loadTextFile docRoot name = loadDocFile docRoot name "_text.md" <&> fmap T.decodeUtf8

saveTextFile :: FilePath -> DocName -> T.Text -> IO ()
saveTextFile docRoot name text = saveDocFile docRoot name "_text.md" (T.encodeUtf8 text)

---

newHandle :: FilePath -> Repo.Handle
newHandle docRoot' =
    Repo.Handle listDocs loadMeta loadDoc saveDoc deleteDoc loadFile
  where
    docRoot = dropTrailingPathSeparator docRoot'

    listDocs :: IO [DocName]
    listDocs = scanDirectory docRoot <&> (\ps -> [docName | (docName, docFile) <- ps, docFile == "_text.md"])

    loadMeta :: DocName -> IO (Maybe DocMeta)
    loadMeta = loadMetaFile docRoot

    loadDoc :: DocName -> IO (Maybe Doc)
    loadDoc name = do
      meta <- loadMetaFile docRoot name
      text <- loadTextFile docRoot name
      pure $ Doc <$> (meta <|> Just (DocMeta name [])) <*> text

    saveDoc :: Doc -> IO Doc
    saveDoc doc = do
      saveMetaFile docRoot (doc ^. meta . name) (doc ^. meta)
      saveTextFile docRoot (doc ^. meta . name) (doc ^. text)
      pure doc

    deleteDoc :: DocName -> IO ()
    deleteDoc name = undefined

    loadFile :: DocName -> DocFile -> IO (Maybe B.ByteString)
    loadFile = loadDocFile docRoot

withHandle :: FilePath -> (Repo.Handle -> IO a) -> IO a
withHandle docRoot action = action (newHandle docRoot)

---

jsonConfig = defaultOptions { fieldLabelModifier = fieldLabelModifier }
  where
    fieldLabelModifier ('d':'t':'o':str) = T.unpack . T.toLower . T.pack $ str

newtype MetaDto = MetaDto { dtoTags :: [T.Text] } deriving (Generic, Eq, Show)

instance ToJSON MetaDto where toJSON = genericToJSON jsonConfig
instance FromJSON MetaDto where parseJSON = genericParseJSON jsonConfig

fromMetaDto name metaDto = DocMeta name (dtoTags metaDto)
toMetaDto meta = MetaDto (meta ^. tags)
