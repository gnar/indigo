module Indigo.WikiTag
  ( parseWikiTag
  , renderWikiTag
  , processWikiText
  ) where

import Prelude hiding (takeWhile)
import Control.Lens ((^.))
import qualified Data.Text as T
import Data.Attoparsec.Text
import Replace.Attoparsec.Text (streamEdit)

import Indigo.WikiEnv
import Control.Applicative ((<|>))

data WikiTag =
    WikiPageRef T.Text T.Text
  | WikiImage T.Text T.Text
  deriving (Eq, Show)

parseWikiTag :: Parser WikiTag
parseWikiTag = imageTag <|> refTag
  where
    sep = char '|'
    token = takeWhile1 $ notInClass "#| \t\n\r"
    tokens = sepBy1 token sep

    refTag = string "#" *> (toRef <$> tokens)
    imageTag = string "#!" *> (toImage <$> tokens)

    toRef [doc] = WikiPageRef doc doc
    toRef [doc, text] = WikiPageRef doc text
    toImage [doc] = WikiImage doc ""
    toImage [doc, text] = WikiImage doc text

renderWikiTag :: WikiTag -> T.Text
renderWikiTag (WikiImage image text) = mconcat [ "![", text, "](", image, ")" ]
renderWikiTag (WikiPageRef ref text) = mconcat [ "[", text, "](", ref, ")" ]

processWikiText :: T.Text -> T.Text
processWikiText = streamEdit parseWikiTag renderWikiTag

