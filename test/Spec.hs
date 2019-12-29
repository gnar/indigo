import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)

import Indigo.WikiEnv
import Indigo.Page
import qualified Indigo.Index as Index

import Data.Function ((&))

testIndex =
  describe "testIndex" $ do
    let empty = Index.empty
        meta1 = Meta "a1" "a1.md" DocTypePage ["tag1", "tag2"]
        meta2 = Meta "a2" "a2.png" DocTypeImage [ "tag1", "tag3"]
        indexA = empty & Index.update meta1 & Index.update meta2
        indexB = indexA & Index.remove "a1"
    describe "emptyIndex" $ do
      it "findAllTags" $ Index.findAllTags' empty `shouldMatchList` []
      it "findAllNames" $ Index.findAllNames' empty `shouldMatchList` []
    describe "indexA" $ do
      it "findAllTags" $ do
        Index.findAllTags' indexA `shouldMatchList` ["tag1", "tag2", "tag3"]
        Index.findAllTags' indexB `shouldMatchList` ["tag1", "tag3"]
      it "findAllNames" $ Index.findAllNames' indexA `shouldMatchList` ["a1", "a2"]
      it "findByTag" $ do
        Index.findByTag' "tag1" indexA `shouldMatchList` [meta1, meta2]
        Index.findByTag' "tag2" indexA `shouldMatchList` [meta1]
        Index.findByTag' "tag3" indexA `shouldMatchList` [meta2]
    describe "indexB" $ do
      it "findAllTags" $ Index.findAllTags' indexB `shouldMatchList` ["tag1", "tag3"]
      it "findAllNames" $ Index.findAllNames' indexB `shouldMatchList` ["a2"]
      it "findByTag" $ do
        Index.findByTag' "tag1" indexB `shouldMatchList` [meta2]
        Index.findByTag' "tag2" indexB `shouldMatchList` []
        Index.findByTag' "tag3" indexB `shouldMatchList` [meta2]

main :: IO ()
main = hspec testIndex
