module Unit.Domain.SearchTokenSpec (spec) where

import Control.Monad (forM_)
import Data.Time (getCurrentTime)
import Domain.Common (Timeline (..))
import Domain.SearchToken (ContentType (Article, Memo), SearchToken (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "ContentType" $ do
    context "show" $ do
      forM_
        [ (Article, "article"),
          (Memo, "memo")
        ]
        $ \(input, expected) ->
          it ("returns" <> expected) $ do
            show input `shouldBe` expected

  describe "SearchToken" $ do
    context "instantiate" $ do
      it "successfully with valid values" $ do
        let identifier' = "ngram:hoge"
            reference' = "01KHN66EYGXEDHTB4VRMGC4A4Z"
            contentType' = Article
            value' = "hoge"
        createdAt' <- getCurrentTime
        updatedAt' <- getCurrentTime
        let timeline' = Timeline createdAt' updatedAt'
            actual =
              SearchToken
                { identifier = identifier',
                  reference = reference',
                  contentType = contentType',
                  value = value',
                  timeline = timeline'
                }
        actual.identifier `shouldBe` identifier'
        actual.reference `shouldBe` reference'
        actual.contentType `shouldBe` contentType'
        actual.value `shouldBe` value'
        actual.timeline `shouldBe` timeline'
