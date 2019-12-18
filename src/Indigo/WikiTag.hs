module Indigo.WikiTag where

import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Attoparsec.Text as A

import Replace.Attoparsec.Text (streamEdit)

import Indigo.WikiEnv

data WikiTag =
    WikiPageLink T.Text T.Text
  | WikiHashTag [T.Text]
  | WikiError [T.Text]
  deriving (Eq, Show)

parseWikiTag :: Parser WikiTag
parseWikiTag =
  parse >>= \case
    [""] -> pureWikiError []
    ["page", page] -> pureWikiPageLink page page
    ["page", page, text] -> pureWikiPageLink page text
    ("tag":tags) -> pure $ WikiHashTag tags
    [page] -> pureWikiPageLink page page
    [page, text] -> pureWikiPageLink page text
    tokens -> pureWikiError tokens
  where
    parse = char '{' *> (A.takeWhile (`notElem` ['}', '|']) `sepBy` char '|') <* char '}'
    pureWikiError tokens = pure $ WikiError tokens
    pureWikiPageLink page text = pure $ WikiPageLink page text

pageUrl :: WikiEnv -> T.Text -> T.Text
pageUrl env name =  mconcat [env ^. host, "/pages/", name]

pagesUrl :: WikiEnv -> T.Text
pagesUrl env =  mconcat [env ^. host, "/pages"]

tagUrl :: WikiEnv -> T.Text -> T.Text
tagUrl env tag =  mconcat [env ^. host, "/tags/", tag]

tagsUrl :: WikiEnv -> T.Text
tagsUrl env =  mconcat [env ^. host, "/tags"]

renderWikiTag :: WikiEnv -> WikiTag -> T.Text
renderWikiTag _ (WikiError tokens) = "{" <> T.intercalate "|" tokens <> "}"
renderWikiTag _ (WikiHashTag _) = ""
renderWikiTag e (WikiPageLink page text) = mconcat ["<a href=\"", pageUrl e page, "\">", text, "</a>"]

processWikiText :: WikiEnv -> T.Text -> T.Text
processWikiText env = streamEdit parseWikiTag (renderWikiTag env)
