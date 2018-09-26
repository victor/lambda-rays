-- | Example of a library file. It is also used for testing the test suites.
module Quadruple
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
  ) where



data Quadruple a = Quadruple { x, y, z, w :: a } deriving (Eq, Show)

qmap :: (Floating a) => Quadruple a -> (a -> a) -> Quadruple a
qmap q fn = fromList $ map fn (toList q)

toList :: (Floating a) => Quadruple a -> [a]
toList Quadruple { x = x1,  y = y1, z = z1, w = w1 } = [x1, y1, z1, w1]

fromList :: (Floating a) => [a] -> Quadruple a
fromList [a, b, c, d] = (Quadruple {x = a, y = b, z = c, w = d})
fromList _ = undefined

point :: (Floating a) => a -> a -> a -> Quadruple a
point a b c = Quadruple {x = a, y = b, z = c, w = 1.0}

vector :: (Floating a) => a -> a -> a -> Quadruple a
vector a b c = Quadruple {x = a, y = b, z = c, w = 0.0}

isPoint :: (Floating a, Eq a) => Quadruple a -> Bool
isPoint Quadruple{w = f} = case f of 1.0  -> True
                                     _ -> False


isVector :: (Floating a, Eq a) => Quadruple a -> Bool
isVector q = w q == 0.0

(*|) :: (Floating a) => a -> Quadruple a -> Quadruple a
(*|) f q1 = qmap q1 (* f)

(|*) :: (Floating a) => Quadruple a -> a -> Quadruple a
(|*) q1 f = f *| q1

(÷) :: (Floating a) => Quadruple a -> a -> Quadruple a
(÷) q1 d = qmap q1 (/ d)


instance Floating a => Num (Quadruple a) where
  q1 + q2 = fromList (zipWith (+) (toList (q1)) (toList (q2)))
  Quadruple _ _ _ _  * Quadruple _ _ _ _ = undefined
  abs (Quadruple _ _ _ _) = undefined
  signum (Quadruple _ _ _ _) = undefined
  fromInteger _ = undefined
  negate a = qmap a negate


magnitude :: (Floating a) => Quadruple a -> a
magnitude q = sqrt ((x q)**2 + (y q)**2 + (z q)**2)

normalize :: (Floating a) => Quadruple a -> Quadruple a
normalize q = qmap q ( / magnitude q)

dot :: (Floating a) => Quadruple a -> Quadruple a -> a
dot a b = ((x a) * (x b)) +
  ((y a) * (y b)) +
  ((z a) * (z b)) +
  ((w a) * (w b))

(⨯) :: (Floating a) => Quadruple a -> Quadruple a -> Quadruple a
(⨯) a b = vector (((y a)*(z b))-((z a)*(y b))) (((z a)*(x b))-((x a)*(z b))) (((x a)*(y b))-((y a)*(x b)))
