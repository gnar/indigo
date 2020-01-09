module Indigo.Service.Ops
  ( loadPage
  , savePage
  , newPage
  , deletePage
  ) where

import Text.Pandoc.Walk (walk)
import qualified Text.Pandoc as P
import qualified Data.Text as T
import Control.Lens

import Indigo.Page
import Indigo.Service.Repo as Repo

import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Either (fromRight)
import Control.Applicative ((<|>))
import Data.Foldable (for_)
import System.FilePath ((</>), takeExtension)
import Network.URI

readMarkdown :: T.Text -> P.Pandoc
readMarkdown text = fromRight undefined (P.runPure (P.readMarkdown rdOpts text))
  where
    extensions = P.pandocExtensions <> P.extensionsFromList [P.Ext_lists_without_preceding_blankline, P.Ext_yaml_metadata_block]
    rdOpts = P.def { P.readerExtensions = extensions }

type Link = ([P.Inline], P.Target)

toPage :: Name -> T.Text -> (Page, P.Pandoc, T.Text)
toPage name text = (page, pandoc, text)
  where
    pandoc@(P.Pandoc meta blocks) = rewriteLinks (readMarkdown text)
    page = Page name meta tags
    tags = maybe [] extractTags (P.lookupMeta "tags" meta)

    extractTags (P.MetaList items) | tags <- [tag | (P.MetaInlines [P.Str tag]) <- items] = fmap T.pack tags
    extractTags _ = []

    rewriteLinks :: P.Pandoc -> P.Pandoc
    rewriteLinks = walk transInline
      where
        transInline (P.Link attr inlines (url, title)) | (inlines', (url', title')) <- f1 (inlines, (url, title)) = P.Link attr inlines' (url', title')
        transInline (P.Image attr inlines (url, title)) | (inlines', (url', title')) <- f2 (inlines, (url, title)) = P.Image attr inlines' (url', title')
        transInline link = link

        f1 ([], ([], title)) = ([], ([], title))
        f1 (text@[P.Str url], ([], title)) = f1 (text, (url, title))
        f1 ([], (url, title)) = f1 ([P.Str url], (url, title))
        f1 (text, (url, title))
          | isExternal url = (text ++ externalSymbol, (url, title))
          | otherwise = (text, (url, title))
          where
            isExternal url | (Just uri) <- parseURIReference url = uriIsAbsolute uri
            isExternal _ = False
            externalSymbol = [P.Space, P.Span ("", ["fa", "fa-external-link-alt"], []) []]

        f2 (text, (url, title))
          | (Just uri) <- parseURIReference url, uriIsRelative uri = (text, (show uri { uriPath = "raw/" <> uriPath uri }, title))
          | otherwise = (text, (url, title))

newPage :: Name -> (Page, P.Pandoc, T.Text)
newPage name = toPage name text
  where
    text = T.unlines
      [ "---"
      , "tags: []"
      , "---"
      , "# " <> name
      , ""
      , "Your text here..."
      ]

loadPage :: Repo.Handle -> Name -> IO (Maybe (Page, P.Pandoc, T.Text))
loadPage repo name = loadTextFile repo (T.unpack name <> ".md") <&> fmap (toPage name)

savePage :: Repo.Handle -> Name -> T.Text -> IO (Page, P.Pandoc, T.Text)
savePage repo name text = do
  saveTextFile repo (T.unpack name <> ".md") text
  return $ toPage name text

deletePage :: Repo.Handle -> Name -> IO ()
deletePage repo name = Repo.deleteFile repo (T.unpack name <> ".md")
