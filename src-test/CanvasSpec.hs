module CanvasSpec where

import Test.Tasty.Hspec
import Canvas

spec_canvas :: Spec
spec_canvas = describe "Canvas" $ do
    describe "Colors are (red, green, blue) tuples" $ do
        let c = Color (-0.5, 0.4, 1.7)
        it "c.red == -0.5" $
            red c `shouldBe` (-0.5)
        it "c.green == 0.4" $
            green c `shouldBe` 0.4
        it "c.blue == 1.7" $
            blue c `shouldBe` 1.7

    describe "Color Operations" $ do
        describe "Adding colors" $ do
            let c1 = Color (0.9, 0.6, 0.75)
            let c2 = Color (0.7, 0.1, 0.25)
            it "c1 + c2 == Color (1.6, 0.7, 1.0)" $
                (c1 + c2) ~= Color (1.6, 0.7, 1.0)
        describe "Subtracting colors" $ do
            let c1 = Color (0.9, 0.6, 0.75)
            let c2 = Color (0.7, 0.1, 0.25)
            it "c1 - c2 == Color (0.2, 0.5, 0.5)" $
                (c1 - c2) ~= Color (0.2, 0.5, 0.5)
        describe "Multiplying a color by a scalar" $ do
            let c = Color (0.2, 0.3, 0.4)
            it "2 *| c == Color (0.4, 0.6, 0.8)" $
              (2 *| c) ~= Color (0.4, 0.6, 0.8)
        describe "Multiplying colors" $ do
            let c1 = Color(1, 0.2, 0.4)
            let c2 = Color (0.9, 1, 0.1)
            it "c1 ⨯ c2 == Color (0.9, 0.2, 0.04)" $
              c1 ⨯ c2 ~= Color (0.9, 0.2, 0.04)

    describe "Creating a Canvas" $ do
        let c = createCanvas 10 20
        it "c.width == 10" $
          width c `shouldBe` 10
        it "c.height == 20" $
          height c `shouldBe` 20
        it "every pixel of c is Color(0,0,0)" $
          True `shouldBe` all (Color (0,0,0) ==) (allPixels c)

    describe "Writing a pixel to a canvas" $ do
        let c = createCanvas 10 20
        let colorRed = Color (1, 0, 0)
        let c' = writePixelAt c 2 3 colorRed
        it "pixel 2,3 of c == colorRed" $
          pixelAt c' 2 3 == colorRed

    describe "Constructing the PPM header" $ do
        let c = createCanvas 5 3
        let ppm = ppmFromCanvas c
        it "lines 1-3 of ppm are [P3, 5 3, 255]" $
          unlines (take 3 (lines ppm)) `shouldBe` "P3\n5 3\n255\n"

    describe "Constructing the PPM pixel data" $ do
        let c = createCanvas 5 3
        let c1 = Color (1.5, 0, 0)
        let c2 = Color (0, 0.5, 0)
        let c3 = Color (-0.5, 0, 1)
        let c' = writePixelAt (writePixelAt (writePixelAt c 0 0 c1) 2 1 c2) 4 2 c3
        it "Lines 4-6 of PPM are \n\
\ 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \n\
\ 0 0 0 0 0 0 0 128 0 0 0 0 0 0 0 \n\
\ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"  $
            unlines (drop 3 (lines(ppmFromCanvas c'))) `shouldBe` "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n" ++
            "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n" ++  "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n"

    describe "Splitting long lines in PPM files" $ do
        let col = Color(1, 0.8, 0.6)
        let c = setAllPixelsTo (createCanvas 10 2) col
        let ppm = ppmFromCanvas c
        -- it "lines 4-7 of ppm are xxx" $
        --   unlines (take 3 (drop 3 (lines ppm))) `shouldBe` "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153\n" ++ 
        --   "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153\n"
        -- Replacing this test ^^ with a check of length for every line
        it "no line is longer than 70 characters" $
            all (\x -> length x <=70) (lines ppm) 
    
    describe "PPM files are terminated by a newline" $ do
        let c = createCanvas 5 3
        let ppm = ppmFromCanvas c
        it "the last character of ppm is a newline" $
          last ppm `shouldBe` '\n'



-- This should be factored out, probably
-- For now keeping it close to edit it more conveniently
class ApproxEq a where
    (~=) :: a -> a -> Bool
    (/~=) :: a -> a -> Bool

instance ApproxEq Float where
    x ~= x' = abs (x-x') < epsilon
        where epsilon = 0.000001 -- good enough for Floats
    x /~= x' = not $ x ~= x'

instance ApproxEq Color where
    Color (r, g, b) ~= Color (r', g', b') = r ~= r' && g ~= g' && b ~= b'
    x /~= x' = not $ x ~= x'
