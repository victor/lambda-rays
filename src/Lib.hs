-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    Quadruple(..),
    point,
    isPoint,
    vector,
    isVector
  ) where



data Quadruple = Quadruple { x, y, z, w :: Double } deriving (Show)


point :: Double -> Double -> Double -> Quadruple
point a b c = Quadruple {x = a, y = b, z = c, w = 1.0}

vector :: Double -> Double -> Double -> Quadruple
vector a b c = Quadruple {x = a, y = b, z = c, w = 0.0}

isPoint :: Quadruple -> Bool
isPoint q = w q == 1.0


isVector :: Quadruple -> Bool
isVector q = w q == 0.0


