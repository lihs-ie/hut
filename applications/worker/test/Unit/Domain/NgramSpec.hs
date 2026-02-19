module Unit.Domain.NgramSpec (spec) where

import Domain.Ngram (generateNgramsBySize)
import Test.Hspec

spec :: Spec
spec = do
  describe "generateNgramsBySize" $ do
    it "generates bigrams and trigrams" $ do
      let result = generateNgramsBySize 2 3 "abcd"
      length result `shouldBe` 2
      head result `shouldMatchList` ["ab", "bc", "cd"]
      result !! 1 `shouldMatchList` ["abc", "bcd"]

    context "when given empty string" $ do
      it "returns empty lists for all sizes" $ do
        generateNgramsBySize 2 4 "" `shouldBe` [[], [], []]

    context "when text is shorter than min ngram size" $ do
      it "returns empty lists" $ do
        generateNgramsBySize 3 4 "ab" `shouldBe` [[], []]
