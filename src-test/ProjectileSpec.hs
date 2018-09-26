module ProjectileSpec where

import Test.Tasty.Hspec

import Projectile
import Quadruple

{-
main :: IO ()
main = do
  tree <- testSpec "hspec tests" testSuite
  defaultMain tree
-}

spec_projectile :: Spec
spec_projectile = do
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

