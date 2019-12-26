module Indigo.Service.Ops where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import Control.Lens

import Indigo.Page
import Indigo.WikiTag

import Indigo.Service.Repo as Repo
import Data.Maybe (maybe, fromJust, fromMaybe)
import Data.Either (fromRight)
import Control.Applicative ((<|>))
import Data.Foldable (for_)

readMarkdown :: T.Text -> Maybe P.Pandoc
readMarkdown rawText = Just $ fromRight undefined (P.runPure (P.readMarkdown rdOpts text))
  where
    text = processWikiText rawText
    extensions = P.pandocExtensions <> P.extensionsFromList [P.Ext_lists_without_preceding_blankline, P.Ext_yaml_metadata_block]
    rdOpts = P.def { P.readerExtensions = extensions }

toPage name text
  | (Just pandoc@(P.Pandoc meta _)) <- readMarkdown text = Just (Page name meta (getTags meta), pandoc, text)
  | otherwise = Nothing
  where
    getTags meta = case P.lookupMeta "tags" meta of
      (Just (P.MetaList x)) | tags <- [tag | (P.MetaInlines [P.Str tag]) <- x] -> T.pack <$> tags
      _ -> []

loadPage :: Repo.Handle -> Name -> IO (Maybe (Page, P.Pandoc, T.Text))
loadPage repo name = loadTextFile repo name "_text.md" <&> (>>= toPage name)

savePage :: Repo.Handle -> Name -> T.Text -> IO (Maybe (Page, P.Pandoc, T.Text))
savePage repo name text = do
  saveTextFile repo name "_text.md" text
  return $ toPage name text

newPage :: Name -> (Page, P.Pandoc, T.Text)
newPage name = fromJust (toPage name text)
  where
    text = T.unlines
      [ "---"
      , "tags: []"
      , "---"
      , "# " <> name
      , ""
      , "Your text here..."
      ]

deletePage :: Repo.Handle -> Name -> IO ()
deletePage repo name = do
  files <- Repo.listFiles repo <&> filter ((== name) . fst) <&> fmap snd
  for_ files $ \file -> do
    Repo.deleteFile repo name file
    pure ()
