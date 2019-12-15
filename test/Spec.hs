import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)

import Indigo.WikiTag
import Indigo.WikiEnv

import Data.Attoparsec.Text (parseOnly)

testWikiTags = describe "WikiTags" $ do

  describe "parseWikiTag" $ do
    it "handles parse errors" $ do
      parseOnly parseWikiTag "some text" `shouldBe` (Left "expected '->': string")

    it "parses pagelinks" $ do
      parseOnly parseWikiTag "->Hauptseite" `shouldBe` (Right $ WikiPageLink "Hauptseite" "Hauptseite")
      parseOnly parseWikiTag "->Hauptseite:here" `shouldBe` (Right $ WikiPageLink "Hauptseite" "here")
      parseOnly parseWikiTag "->(Hauptseite)" `shouldBe` (Right $ WikiPageLink "Hauptseite" "Hauptseite")
      parseOnly parseWikiTag "->(Hauptseite:the main page)" `shouldBe` (Right $ WikiPageLink "Hauptseite" "the main page")

  describe "renderWikiTag" $ do
    let env = WikiEnv { _host = "http://example.com", _pageDir = "./pages"}

    it "renders errors" $ do
      renderWikiTag env (WikiError ["one", "two"]) `shouldBe` "`->one:two`"

    it "renders links" $ do
      renderWikiTag env (WikiPageLink "Main Page" "the main page") `shouldBe` "[the main page](http://example.com/Main Page)"


main :: IO ()
main = hspec testWikiTags
