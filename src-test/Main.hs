import Test.Tasty
import Test.Tasty.Hspec

import Quadruple
import Projectile

main :: IO ()
main = do
  tree <- testSpec "hspec tests" testSuite
  defaultMain tree


testSuite :: Spec
testSuite = do
  describe "Tuples components" $ do
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
        p `shouldBe` Quadruple 4 (-4) 3 1

    describe "'vector' describes tuples with w=0" $ do
      let v = vector 4 (-4) 3
      it "v = tuple(4, -4, 3, 0)" $
        v `shouldBe` Quadruple 4 (-4) 3 0

  describe "Tuple Arithmetics" $ do
    describe "Quadruple addition" $ do
      let a1 = Quadruple 3 (-2) 5 1
      let a2 = Quadruple (-2) 3 1 0
      it "a1 + a2 = (1, 1, 6, 1)" $
        a1 + a2 `shouldBe` Quadruple 1 1 6 1

    describe "Subtracting two points" $ do
      let p1 = point 3 2 1
      let p2 = point 5 6 7
      it "p1 - p2 == vector (-2, -4, -6)" $
        p1 - p2 `shouldBe` vector (-2) (-4) (-6)

    describe "Subtracting a vector from a point" $ do
      let p = point 3 2 1
      let v = vector 5 6 7
      it "p - v == point(-2, -4, -6)" $
        p - v `shouldBe` point (-2) (-4) (-6)

    describe "Subtracting two vectors" $ do
      let v1 = vector 3 2 1
      let v2 = vector 5 6 7
      it "v1 - v2 == vector(-2, -4, -6)" $
        v1 - v2 `shouldBe` vector (-2) (-4) (-6)

    describe "Subtracting a vector from the zero vector" $ do
      let zero = vector 0 0 0
      let v = vector 1 (-2) 3
      it "zero - v = vector(-1, 2, -3)" $
        zero - v `shouldBe` vector (-1) 2 (-3)

    describe "Negating a tuple" $ do
      let a = Quadruple 1 (-2) 3 (-4)
      it "-a = tuple (-1, 2, -3, 4)" $
        (-a) `shouldBe` Quadruple (-1) 2 (-3) 4

    describe "Multiplying a tuple by a scalar" $ do
      let a = Quadruple 1 (-2) 3 (-4)
      it "3.5 *| a = tuple(3.5, -7, 10.5, -14)" $
        3.5 *| a `shouldBe` Quadruple 3.5 (-7) 10.5 (-14)
      describe "Scalar by Tuple" $ do
        it "3.5 *| a = tuple(3.5, -7, 10.5, -14)" $
          3.5 *| a `shouldBe` Quadruple 3.5 (-7) 10.5 (-14)
      describe "Tuple by Scalar" $ do
        it "a *| 3.5 = tuple(3.5, -7, 10.5, -14)" $
          a |* 3.5 `shouldBe` Quadruple 3.5 (-7) 10.5 (-14)

    describe "Multiplying a tuple by a fraction" $ do
      let a = Quadruple 1 (-2) 3 (-4)
      it "a * 0.5 = tuple(0.5, -1, 1.5, -2)" $
        a |* 0.5 `shouldBe` Quadruple 0.5 (-1) 1.5 (-2)

    describe "Dividing a tuple by a scalar" $ do
      let a = Quadruple 1 (-2) 3 (-4)
      it "a ÷ 2 =  tuple(0.5, -1, 1.5, -2)" $
        a ÷ 2 `shouldBe` Quadruple 0.5 (-1) 1.5 (-2)

  describe "Magnitude" $ do
    describe "Magnitude of vector(1, 0, 0)" $ do
      let v = vector 1 0 0
      it "magnitude(v) = 1" $
        magnitude v `shouldBe` 1.0

    describe "Magnitude of vector(0, 1, 0)" $ do
      let v = vector 0 1 0
      it "magnitude(v) = 1" $
        magnitude v `shouldBe` 1.0

    describe "Magnitude of vector(0, 0, 1)" $ do
      let v = vector 0 0 1
      it "magnitude(v) = 1" $
        magnitude v `shouldBe` 1.0

    describe "Magnitude of vector(1, 2, 3)" $ do
      let v = vector 1 2 3
      it "magnitude(v) = √14" $
        magnitude v `shouldBe` sqrt 14

    describe "Magnitude of vector(-1, -2, -3)" $ do
      let v = vector (-1) (-2) (-3)
      it "magnitude(v) = √14" $
        magnitude v `shouldBe` sqrt 14

  describe "Vector normalization" $ do

    describe "Normalizing vector(4, 0, 0) gives (1, 0, 0)" $ do
      let v = vector 4 0 0
      it "normalize(v) = vector(1, 0, 0)" $
        normalize v `shouldBe` vector 1 0 0
    describe "Normalizing vector(1, 2, 3)" $ do
      let v = vector 1 2 3
      it "normalize(v) = ~= vector(0.26726, 0.53452, 0.80178)" $
        normalize v `shouldBe` vector 0.2672612419124244 0.5345224838248488 0.8017837257372732
    describe "The magnitude of a normalized vector" $ do
      let v = vector 1 2 3
      let n = normalize v
      it "magnitude(norm) = 1" $
        magnitude n `shouldBe` 1

  describe "Dot Product" $ do
    describe "The dot product of two tuples" $ do
      let a = vector 1 2 3
      let b = vector 2 3 4
      it "dot(a, b) = 20" $
        (a `dot` b) `shouldBe` 20

  describe "Cross Product" $ do
    describe "Cross product of two vectors" $ do
      let a = vector 1 2 3
      let b = vector 2 3 4
      it "a ⨯ b = vector(-1, 2, -1)" $
        a ⨯ b `shouldBe` vector (-1) 2 (-1)
      it "b ⨯ a = vector(1, -2, 1)" $
        b ⨯ a `shouldBe` vector 1 (-2) 1

  describe "Projectile" $ do
    describe "initial moment" $ do
      let world = World{ gravity = vector 0 (-9.8) 0, wind = vector 0 0 0}
      let p = Projectile { position = point 0 9.8 0, velocity = vector 0 0 0}
      it "After 0 second without velocity, position shouldn't change, velocity has increased" $ do
        let p1 = tick world p
        position p1 `shouldBe` point 0 9.8 0
        velocity p1 `shouldBe` vector 0 (-9.8) 0
      it "After 1 second without velocity, position is ground level, velocity has doubled" $ do
        let p1 = tick world (tick world p)
        position p1 `shouldBe` point 0 0 0
        velocity p1 `shouldBe` vector 0 (-19.6) 0

