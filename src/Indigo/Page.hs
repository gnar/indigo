module Indigo.Page where

import GHC.Generics
import qualified Data.Text as T

import System.FilePath
import System.Directory
import Control.Lens hiding (($))

import Indigo.WikiEnv
import Indigo.WikiTag

import Text.Pandoc
import qualified Text.Blaze.Html5 as H

data Page = Page {
  _name :: T.Text,
  _text :: T.Text
} deriving (Generic, Show)

name :: Lens' Page T.Text
name = lens _name (\p n -> p { _name = n })

text :: Lens' Page T.Text
text = lens _text (\p t -> p { _text = t })

pageFile :: WikiEnv -> T.Text -> T.Text
pageFile e name = _pageDir e <> "/" <> name <> ".md"

loadPage :: WikiEnv -> T.Text -> IO (Maybe Page)
loadPage e name = do
    exists <- doesFileExist file
    if exists then do
      text <- T.pack <$> readFile file
      pure $ Just (Page name text)
    else
      pure Nothing
  where
    file = T.unpack $ pageFile e name

deletePage :: WikiEnv -> T.Text -> IO ()
deletePage e name = removeFile file
  where
    file = T.unpack $ pageFile e name

loadOrCreatePage :: WikiEnv -> T.Text -> IO Page
loadOrCreatePage env name = do
    page <- loadPage env name
    case page of
      Just p -> pure p
      Nothing -> updatePage env page' >> pure page'
  where
    page' = newPage name

updatePage :: WikiEnv -> Page -> IO ()
updatePage e page = writeFile file (T.unpack $ page^.text)
  where
    file = T.unpack $ pageFile e (page ^. name)

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
  ]
}
