module Indigo.WikiTag where

import Control.Lens
import qualified Data.Text as T
import Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Control.Applicative

import System.FilePath ((</>))

import Replace.Attoparsec.Text (streamEdit)

import Indigo.WikiEnv

data WikiTag =
    WikiPageLink T.Text T.Text
  | WikiHashTag [T.Text]
  | WikiError [T.Text]
  deriving (Eq, Show)

parseWikiTag :: Parser WikiTag
parseWikiTag = do
  tokens <- (string "->" <?> "expected '->'") *> (parenTokens <|> tokens) :: Parser [T.Text]
  case tokens of
    [""] -> pureWikiError []

    -- page links
    ["page", page] -> pureWikiPageLink page page
    ["page", page, text] -> pureWikiPageLink page text

    -- hash tags
    ("tag": tags) -> pure $ WikiHashTag tags

    -- default to page link
    [page] -> pureWikiPageLink page page
    [page, text] -> pureWikiPageLink page text

    tokens -> pureWikiError tokens
  where
    parenTokens = char '(' *> (A.takeWhile (`notElem` [')', ':']) `sepBy` char ':') <* char ')'
    tokens = A.takeWhile (not . (\ch -> isSpace ch || ch == ':')) `sepBy` char ':'

    pureWikiError tokens = pure (WikiError tokens)
    pureWikiPageLink page text = pure $ WikiPageLink page text

renderPageLink :: WikiEnv -> T.Text -> T.Text
renderPageLink env page = env ^. host <> "/" <> page

renderWikiTag :: WikiEnv -> WikiTag -> T.Text
renderWikiTag _ (WikiError toks) = "`->" <> T.intercalate ":" toks <> "`"
renderWikiTag _ (WikiHashTag _) = ""
renderWikiTag e (WikiPageLink page text) = mconcat ["<a href=\"", renderPageLink e page, "\">", text, "</a>"]

processWikiText :: WikiEnv -> T.Text -> T.Text
processWikiText env = streamEdit parseWikiTag (renderWikiTag env)
