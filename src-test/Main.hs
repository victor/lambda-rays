import Test.Tasty
import Test.Tasty.Hspec

import Lib 

main :: IO ()
main = do
  tree <- testSpec "hspec tests" testSuite
  defaultMain tree


testSuite :: Spec
testSuite = do
  describe "A tuple with w == 1.0 is a point" $ do
    let p = Quadruple 4.3 (-4.2) 3.1 1.0
    it "x == 4.3" $
     x p `shouldBe` 4.3
    it "y == -4.2" $
     y p `shouldBe` (-4.2)
    it "z == 3.1" $
     z p `shouldBe` 3.1
    it "w == 1.0" $
     w p `shouldBe` 1.0
    it "is a point" $
     isPoint p `shouldBe` True
    it "is not a vector" $
     isVector p `shouldBe` False
  describe "A tuple with w == 0.0 is a vector" $ do
    let v = Quadruple 4.3 (-4.2) 3.1 0.0
    it "x == 4.3" $
     x v `shouldBe` 4.3
    it "y == -4.2" $
     y v `shouldBe` (-4.2)
    it "z == 3.1" $
     z v `shouldBe` 3.1
    it "w == 0.0" $
     w v `shouldBe` 0.0
    it "is not a point" $
     isPoint v `shouldBe` False
    it "is a vector" $
     isVector v `shouldBe` True
  describe "'point' describes tuples with w=1" $ do
    let p = point 4 (-4) 3
    it "p = tuple(4, -4, 3, 1)" $
      p == Quadruple 4 (-4) 3 1
  describe "'vector' describes tuples with w=0" $ do
    let v = vector 4 (-4) 3
    it "v = tuple(4, -4, 3, 0)" $
      v == Quadruple 4 (-4) 3 0
  describe "Quadruple addition" $ do
    let a1 = Quadruple 3 (-2) 5 1
    let a2 = Quadruple (-2) 3 1 0
    it "a1 + a2 = (1, 1, 6, 1)" $
      a1 + a2 == Quadruple 1 1 6 1
  describe "Subtracting two points" $ do
    let p1 = point 3 2 1
    let p2 = point 5 6 7
    it "p1 - p2 == vector (-2, -4, -6)" $
      p1 - p2 == vector (-2) (-4) (-6)

