-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    Quadruple(..),
    point,
    isPoint,
    vector,
    isVector,
    (*|),
    (|*),
    (÷),
    magnitude
    ,normalize
    ,dot
    ,(⨯)
    ,World(..)
    ,Projectile(..)
    ,tick
  ) where



data Quadruple = Quadruple { x, y, z, w :: Double } deriving (Eq, Show)

qmap :: Quadruple -> (Double -> Double) -> Quadruple
qmap q fn = Quadruple (fn (x q)) (fn (y q)) (fn (z q)) (fn (w q))

point :: Double -> Double -> Double -> Quadruple
point a b c = Quadruple {x = a, y = b, z = c, w = 1.0}

vector :: Double -> Double -> Double -> Quadruple
vector a b c = Quadruple {x = a, y = b, z = c, w = 0.0}

isPoint :: Quadruple -> Bool
isPoint Quadruple{w = f} = case f of 1.0  -> True
                                     _ -> False


isVector :: Quadruple -> Bool
isVector q = w q == 0.0

(*|) :: Double -> Quadruple -> Quadruple
(*|) f q1 = qmap q1 (* f)

(|*) :: Quadruple -> Double -> Quadruple
(|*) q1 f = f *| q1

(÷) :: Quadruple -> Double -> Quadruple
(÷) q1 d = qmap q1 (/ d)


instance Num Quadruple where
  Quadruple x1 y1 z1 w1 + Quadruple x2 y2 z2 w2 = Quadruple (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  Quadruple _ _ _ _  * Quadruple _ _ _ _ = undefined
  abs (Quadruple _ _ _ _) = undefined
  signum (Quadruple _ _ _ _) = undefined
  fromInteger _ = undefined
  negate a = qmap a negate

  
magnitude :: Quadruple -> Double
magnitude q = sqrt ((x q)**2 + (y q)**2 + (z q)**2)

normalize :: Quadruple -> Quadruple
normalize q = qmap q ( / magnitude q)

dot :: Quadruple -> Quadruple -> Double
dot a b = ((x a) * (x b)) +
  ((y a) * (y b)) +
  ((z a) * (z b)) +
  ((w a) * (w b))

(⨯) :: Quadruple -> Quadruple -> Quadruple
(⨯) a b = vector (((y a)*(z b))-((z a)*(y b))) (((z a)*(x b))-((x a)*(z b))) (((x a)*(y b))-((y a)*(x b)))

data World = World { gravity, wind :: Quadruple} deriving (Eq, Show)

data Projectile = Projectile { position, velocity :: Quadruple} deriving (Eq, Show)

tick :: World -> Projectile -> Projectile
tick world p0 = Projectile { position = position p0 + velocity p0, velocity = velocity p0 + gravity world + wind world }

