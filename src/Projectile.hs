-- | Example of a library file. It is also used for testing the test suites.
module Projectile
  (
    World(..)
    ,Projectile(..)
    ,tick
  ) where

import Quadruple

data World = World { gravity, wind :: Quadruple} deriving (Eq, Show)

data Projectile = Projectile { position, velocity :: Quadruple} deriving (Eq, Show)

tick :: World -> Projectile -> Projectile
tick world p0 = Projectile { position = position p0 + velocity p0, velocity = velocity p0 + gravity world + wind world }

