import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)

import Indigo.WikiTag
import Indigo.WikiEnv
import Indigo.Page
import qualified Indigo.Index as Index

import Data.Attoparsec.Text (parseOnly)

testIndex =
  describe "testIndex" $ do
    let clear = Index.clear
        meta1 = PageMeta ["tag1", "tag2"]
        meta2 = PageMeta ["tag1", "tag3"]
        indexA = Index.update "a1" meta1 $ Index.update "a2" meta2 Index.empty
        indexB = Index.remove "a1" indexA
    describe "clearIndex" $ do
      it "findAllTags" $ Index.findAllTags clear `shouldBe` []
      it "findAllNames" $ Index.findAllNames clear `shouldBe` []
    describe "indexA" $ do
      it "findAllTags" $ do
        Index.findAllTags indexA `shouldBe` ["tag1", "tag2", "tag3"]
        Index.findAllTags indexB `shouldBe` ["tag1", "tag3"]
      it "findAllNames" $ Index.findAllNames indexA `shouldBe` ["a1", "a2"]
      it "findByTag" $ do
        Index.findByTag "tag1" indexA `shouldBe` [("a1", meta1), ("a2", meta2)]
        Index.findByTag "tag2" indexA `shouldBe` [("a1", meta1)]
        Index.findByTag "tag3" indexA `shouldBe` [("a2", meta2)]
    describe "indexB" $ do
      it "findAllTags" $ Index.findAllTags indexB `shouldBe` ["tag1", "tag3"]
      it "findAllNames" $ Index.findAllNames indexB `shouldBe` ["a2"]
      it "findByTag" $ do
        Index.findByTag "tag1" indexB `shouldBe` [("a2", meta2)]
        Index.findByTag "tag2" indexB `shouldBe` []
        Index.findByTag "tag3" indexB `shouldBe` [("a2", meta2)]

testWikiTags =
  describe "testWikiTags" $ do
    describe "parseWikiTag" $
      it "parses pagelinks" $ do
        parseOnly parseWikiTag "{Hauptseite}" `shouldBe` (Right $ WikiPageLink "Hauptseite" "Hauptseite")
        parseOnly parseWikiTag "{Hauptseite|the main page}" `shouldBe`
          (Right $ WikiPageLink "Hauptseite" "the main page")
    describe "renderWikiTag" $ do
      let env = WikiEnv {_host = "http://example.com", _pageDir = "./pages", _staticDir = "./static"}
      it "renders errors" $ renderWikiTag env (WikiError ["one", "two"]) `shouldBe` "{one|two}"
      it "renders links" $
        renderWikiTag env (WikiPageLink "Main Page" "the main page") `shouldBe` "<a href=\"http://example.com/pages/Main Page\">the main page</a>"

main :: IO ()
main = hspec (testWikiTags >> testIndex)
