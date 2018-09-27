module Canvas (
    Color(..)
    , red
    , green
    , blue
    , (*|)
    , (⨯)
    , createCanvas
    , width
    , height
    -- , matrix
    , allPixels
) where

import Data.Array


-- No point in generalizing the type, Float is more than enough for all the possible colors
data Color = Color (Float, Float, Float) deriving (Eq, Show)

first :: (Float, Float, Float) -> Float
first (t1, _, _) = t1

second :: (Float, Float, Float) -> Float
second (_, t2, _) = t2

third :: (Float, Float, Float) -> Float
third (_, _, t3) = t3


red :: Color -> Float
red (Color t) = first t

green :: Color -> Float
green (Color t) = second t

blue :: Color -> Float
blue (Color t) = third t

(*|) :: Float -> Color -> Color
a *| Color (r1, g1, b1) = Color (a * r1, a * g1, a * b1)


instance Num Color where
    Color (r1, g1, b1) + Color (r2, g2, b2) = Color (r1 + r2, g1 + g2, b1 + b2)
    _ * _ = undefined
    abs (_) = undefined
    signum (_) = undefined
    fromInteger _ = undefined
    negate (Color (r, g, b)) = Color ((-r), (-g), (-b))

(⨯) :: Color -> Color -> Color
Color (r1, g1, b1) ⨯ Color (r2, g2, b2) = Color (r1 * r2, g1 * g2, b1 * b2) -- Hadamard product


-- Pixel matrix

-- For now, let's try an immutable Array type and see how that works.
data Canvas = Canvas (Array (Int, Int) Color) deriving (Show)

createCanvas :: Int -> Int -> Canvas
createCanvas x y = Canvas m where m = array ((0,0), (x-1,y-1)) [ ((i,j), Color (0,0,0)) | i <- [0..x-1], j <- [0..y-1]]
    
width :: Canvas -> Int
width (Canvas m) = 1 + fst ( snd (bounds m))

height :: Canvas -> Int
height (Canvas m) = 1 + snd ( snd (bounds m))

-- accessor for the inner Array
matrix :: Canvas -> Array (Int, Int) Color
matrix (Canvas m) = m

allPixels :: Canvas -> [Color]
allPixels (Canvas m) = elems m