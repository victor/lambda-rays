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



data Quadruple = Quadruple { x, y, z, w :: Double } deriving (Eq, Show)


point :: Double -> Double -> Double -> Quadruple
point a b c = Quadruple {x = a, y = b, z = c, w = 1.0}

vector :: Double -> Double -> Double -> Quadruple
vector a b c = Quadruple {x = a, y = b, z = c, w = 0.0}

isPoint :: Quadruple -> Bool
isPoint Quadruple{w = f} = case f of 1.0  -> True
                                     _ -> False


isVector :: Quadruple -> Bool
isVector q = w q == 0.0

-- (+) :: Quadruple -> Quadruple -> Quadruple
-- (+) q1 q2 = Quadruple (x q1 Prelude.+ x q2) (y q1 Prelude.+ y q2) (z q1 Prelude.+ z q2) (w q1 Prelude.+ w q2)


instance Num Quadruple where
  Quadruple x1 y1 z1 w1 + Quadruple x2 y2 z2 w2 = Quadruple (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  Quadruple _ _ _ _  * Quadruple _ _ _ _ = undefined
  abs (Quadruple _ _ _ _) = undefined
  signum (Quadruple _ _ _ _) = undefined
  fromInteger _ = undefined
  negate (Quadruple x1 y1 z1 w1) = Quadruple (negate x1) (negate y1) (negate z1) (negate w1)