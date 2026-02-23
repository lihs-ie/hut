module Unit.Domain.CommonSpec (spec) where

import Data.Time
import Domain.Common (Timeline (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Timeline" $ do
    context "instantiate" $ do
      it "successfully with valid values" $ do
        created <- getCurrentTime
        updated <- getCurrentTime
        let actual = Timeline created updated
        createdAt actual `shouldBe` created
        updatedAt actual `shouldBe` updated
