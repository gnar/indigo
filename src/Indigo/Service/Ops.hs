module Indigo.Service.Ops
  ( loadPage
  , savePage
  , newPage
  , deletePage
  ) where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import Control.Lens

import Indigo.Page

import Indigo.Service.Repo as Repo
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Either (fromRight)
import Control.Applicative ((<|>))
import Data.Foldable (for_)
import System.FilePath ((</>))

readMarkdown :: T.Text -> P.Pandoc
readMarkdown text = fromRight undefined (P.runPure (P.readMarkdown rdOpts text))
  where
    extensions = P.pandocExtensions <> P.extensionsFromList [P.Ext_lists_without_preceding_blankline, P.Ext_yaml_metadata_block]
    rdOpts = P.def { P.readerExtensions = extensions }

type Link = ([P.Inline], P.Target)

toPage :: Name -> T.Text -> (Page, P.Pandoc, T.Text)
toPage name text = (page, pandoc, text)
  where
    pandoc@(P.Pandoc meta blocks) = rewriteLinks f1 f2 (readMarkdown text)
    page = Page name meta tags
    tags = maybe [] extractTags (P.lookupMeta "tags" meta)

    f1, f2 :: Link -> Link
    f1 (text, (url, title)) = if null text then f1 ([P.Str url], (url, title)) else (text, (url, title))
    f2 = id

    extractTags (P.MetaList items) | tags <- [tag | (P.MetaInlines [P.Str tag]) <- items] = fmap T.pack tags
    extractTags _ = []

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

rewriteLinks :: (Link -> Link) -> (Link -> Link) -> P.Pandoc -> P.Pandoc
rewriteLinks f1 f2 = transPandoc
  where
    transPandoc :: P.Pandoc -> P.Pandoc
    transPandoc (P.Pandoc meta blocks) = P.Pandoc meta (transBlocks blocks)

    transBlock :: P.Block -> P.Block
    transBlock (P.Plain inlines) = P.Plain (transInlines inlines)
    transBlock (P.Para inlines) = P.Para (transInlines inlines)
    transBlock (P.LineBlock inliness) = P.LineBlock (fmap transInlines inliness)
    transBlock (P.BlockQuote blocks) = P.BlockQuote (transBlocks blocks)
    transBlock (P.OrderedList attributes blockss) = P.OrderedList attributes (fmap transBlocks blockss)
    transBlock (P.BulletList blockss) = P.BulletList (fmap transBlocks blockss)
    transBlock (P.DefinitionList entries) = P.DefinitionList [(fmap transInline inlines, fmap transBlocks blockss) | (inlines, blockss) <- entries]
    transBlock (P.Header level attr inlines) = P.Header level attr (transInlines inlines)
    transBlock (P.Table inlines aligns relColWidths headers rows) = P.Table (transInlines inlines) aligns relColWidths (fmap transBlocks headers) (fmap (fmap transBlocks) rows)
    transBlock (P.Div attr blocks) = P.Div attr (transBlocks blocks)
    transBlock block = block
    transBlocks = fmap transBlock

    transInline :: P.Inline -> P.Inline
    transInline (P.Emph inlines) = P.Emph (transInlines inlines)
    transInline (P.Strong inlines) = P.Strong (transInlines inlines)
    transInline (P.Strikeout inlines) = P.Strikeout (transInlines inlines)
    transInline (P.Superscript inlines) = P.Superscript (transInlines inlines)
    transInline (P.Subscript inlines) = P.Subscript (transInlines inlines)
    transInline (P.SmallCaps inlines) = P.SmallCaps (transInlines inlines)
    transInline (P.Quoted qt inlines) = P.Quoted qt (transInlines inlines)
    transInline (P.Cite cites inlines) = P.Cite cites (transInlines inlines) -- todo: transCitation
    transInline (P.Link attr inlines (url, title)) | (inlines', (url', title')) <- f1 (inlines, (url, title)) = P.Link attr inlines' (url', title')
    transInline (P.Image attr inlines (url, title)) | (inlines', (url', title')) <- f2 (inlines, (url, title)) = P.Image attr inlines' (url', title')
    transInline (P.Note blocks) = P.Note (transBlocks blocks)
    transInline (P.Span attr inlines) = P.Span attr (transInlines inlines)
    transInline inline = inline
    transInlines = fmap transInline
