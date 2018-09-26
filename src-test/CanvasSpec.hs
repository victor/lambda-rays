module CanvasSpec where

import Test.Tasty.Hspec

import Canvas

spec_canvas :: Spec
spec_canvas = do
  describe "Canvas" $ do
    describe "Colors are (red, green, blue) tuples" $ do
        let c = Color ((-0.5), 0.4, 1.7)
        it "c.red == -0.5" $
            red c `shouldBe` (-0.5)
        it "c.green == 0.4" $
            green c `shouldBe` 0.4
        it "c.blue == 1.7" $
            blue c `shouldBe` 1.7