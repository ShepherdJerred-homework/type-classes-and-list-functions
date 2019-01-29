import Lib
import Test.Hspec

main :: IO ()

main = hspec $ do
  describe "dotProduct" $ do
    it "calculates the dot product" $ do
      dotProduct (2, 6) (3, 1) `shouldBe` 12
  describe "vecLength" $ do
    it "calculates a 2d vector length" $ do
      vecLength (3, 4) `shouldBe` 5
  describe "isUnique" $ do
    it "returns true if a list is unique" $ do
      isUnique ([] :: [Int]) `shouldBe` True
      isUnique [ 1 ] `shouldBe` True
      isUnique [ 1, 2 ] `shouldBe` True
      isUnique [1, 2, 3] `shouldBe` True
      isUnique [ 4, 1, 5, 9, 2, 6 ] `shouldBe` True
    it "returns false is a list is not unique" $ do
      isUnique [ 3, 1, 4, 1, 5, 9, 2, 6 ] `shouldBe` False
  describe "isAscending" $ do
    it "returns true if a list is ascending" $ do
      isAscending [1, 2, 3, 5, 8, 13] `shouldBe` True
    it "returns false if a list is not ascending" $ do
      isAscending [1, 6, 8, 4, 12, 22] `shouldBe` False
  describe "to" $ do
    it "returns a range" $ do
      3 `to` 7 `shouldBe` [3,4,5,6,7]
      12 `to` 8 `shouldBe` [12,11,10,9,8]
      'p' `to` 'v' `shouldBe` "pqrstuv"
