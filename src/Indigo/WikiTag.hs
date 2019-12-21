module Indigo.WikiTag where

import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Attoparsec.Text as A

import Replace.Attoparsec.Text (streamEdit)

import Indigo.WikiEnv

data WikiTag =
    WikiPageLink T.Text T.Text
  | WikiImage T.Text
  | WikiError [T.Text]
  deriving (Eq, Show)

parseWikiTag :: Parser WikiTag
parseWikiTag =
  parse >>= \case
    [""] -> pureWikiError []
    ["page", page] -> pureWikiPageLink page page
    ["page", page, text] -> pureWikiPageLink page text
    ["image", image] -> pureWikiImage image
    [page] -> pureWikiPageLink page page
    [page, text] -> pureWikiPageLink page text
    tokens -> pureWikiError tokens
  where
    parse = char '{' *> (A.takeWhile (`notElem` ['}', '|']) `sepBy` char '|') <* char '}'
    pureWikiError tokens = pure $ WikiError tokens
    pureWikiPageLink page text = pure $ WikiPageLink page text
    pureWikiImage image = pure $ WikiImage image

renderWikiTag :: WikiEnv -> WikiTag -> T.Text
renderWikiTag _ (WikiError tokens) = "{" <> T.intercalate "|" tokens <> "}"
renderWikiTag e (WikiImage image) = mconcat [ "<img src=\"", pageFileUrl e image, "\"/>" ]
renderWikiTag e (WikiPageLink page text) = mconcat ["<a href=\"", pageUrl e page, "\">", text, "</a>"]

processWikiText :: WikiEnv -> T.Text -> T.Text
processWikiText env = streamEdit parseWikiTag (renderWikiTag env)
