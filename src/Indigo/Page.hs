module Indigo.Page where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.FilePath
import System.Directory
import Control.Lens hiding (($))

import Indigo.WikiEnv
import Indigo.WikiTag

import Text.Pandoc
import qualified Text.Blaze.Html5 as H

import Data.Aeson
import Data.Aeson.Text as AT
import Data.Maybe (fromJust, fromMaybe)

data Page = Page {
  _name :: T.Text,
  _text :: T.Text,
  _meta :: PageMeta
} deriving (Generic, Show)

data PageMeta = PageMeta {
  _tags :: [T.Text]
} deriving (Generic, Show)

deriving instance FromJSON PageMeta
deriving instance ToJSON PageMeta

name :: Lens' Page T.Text
name = lens _name (\p n -> p { _name = n })

text :: Lens' Page T.Text
text = lens _text (\p t -> p { _text = t })

meta :: Lens' Page PageMeta
meta = lens _meta (\p m -> p { _meta = m })

tags :: Lens' PageMeta [T.Text]
tags = lens _tags (\m t -> m { _tags = t })

emptyPageMeta = PageMeta []

pageMetaFile :: WikiEnv -> T.Text -> T.Text
pageMetaFile e name = _pageDir e <> "/" <> name <> ".json"

pageTextFile :: WikiEnv -> T.Text -> T.Text
pageTextFile e name = _pageDir e <> "/" <> name <> ".md"

loadPage :: WikiEnv -> T.Text -> IO (Maybe Page)
loadPage e name = do
  exists <- doesFileExist textFile
  if exists
    then do
      text <- T.decodeUtf8 <$> B.readFile textFile
      meta <- fromMaybe emptyPageMeta . decode . BL.fromStrict <$> B.readFile metaFile
      pure $ Just $ Page name text meta
    else pure Nothing
  where
    textFile = T.unpack $ pageTextFile e name
    metaFile = T.unpack $ pageMetaFile e name

deletePage :: WikiEnv -> T.Text -> IO ()
deletePage e name = removeFile textFile >> removeFile metaFile
  where
    textFile = T.unpack $ pageTextFile e name
    metaFile = T.unpack $ pageMetaFile e name

loadOrCreatePage :: WikiEnv -> T.Text -> IO Page
loadOrCreatePage env name = do
    page <- loadPage env name
    case page of
      Just p -> pure p
      Nothing -> updatePage env page' >> pure page'
  where
    page' = newPage name

updatePage :: WikiEnv -> Page -> IO ()
updatePage e page = do
    B.writeFile textFile $ T.encodeUtf8 (page ^. text)
    B.writeFile metaFile $ BL.toStrict $ encode (page ^. meta)
  where
    textFile = T.unpack $ pageTextFile e (page ^. name)
    metaFile = T.unpack $ pageMetaFile e (page ^. name)

pageHtml :: WikiEnv -> Page -> H.Html
pageHtml env page =
  let
      rdOpts = def { readerExtensions = githubMarkdownExtensions } :: ReaderOptions
      wrOpts = def :: WriterOptions
      text' = processWikiText env (page ^. text)
      res = runPure $ readMarkdown rdOpts text' >>= writeHtml5 wrOpts
  in case res of
      Left err -> undefined
      Right doc -> doc

newPage :: T.Text -> Page
newPage name = Page {
  _name = name,
  _text = T.unlines [
    "# " <> name,
    "",
    "No contents."
  ],
  _meta = PageMeta []
}
