{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Canvas (
    Color(..)
    ,red
    ,green
    ,blue
    ,(*|)
    ,(⨯)
    ,createCanvas
    ,width
    ,height
    -- , matrix
    ,allPixels
    ,pixelAt
    ,writePixelAt
    ,ppmFromCanvas
    ,setAllPixelsTo
) where

import Data.Array


-- No point in generalizing the type, Float is more than enough for all the possible colors
newtype Color = Color (Float, Float, Float) deriving (Eq, Show)

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
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined
    negate (Color (r, g, b)) = Color (-r, -g, -b)

(⨯) :: Color -> Color -> Color
Color (r1, g1, b1) ⨯ Color (r2, g2, b2) = Color (r1 * r2, g1 * g2, b1 * b2) -- Hadamard product


-- Pixel matrix
type PixelMatrix = Array (Int, Int) Color

-- For now, let's try an immutable Array type and see how that works.
newtype Canvas = Canvas PixelMatrix deriving (Show)

createCanvas :: Int -> Int -> Canvas
createCanvas x y = Canvas (array ((0,0), (x-1,y-1)) [ ((i,j), Color (0,0,0)) | i <- [0..x-1], j <- [0..y-1]] )
    
width :: Canvas -> Int
width (Canvas m) = 1 + fst ( snd (bounds m))

height :: Canvas -> Int
height (Canvas m) = 1 + snd ( snd (bounds m))

-- accessor for the inner Array
-- matrix :: Canvas -> PixelMatrix
-- matrix (Canvas m) = m

allPixels :: Canvas -> [Color]
allPixels (Canvas m) = elems m

-- allPixelsInRows :: Canvas -> [[Color]]
-- allPixelsInRows (Canvas m) = 

pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas m) i j = m!(i, j)

writePixelAt :: Canvas -> Int -> Int -> Color -> Canvas
writePixelAt (Canvas m) i j c = Canvas (m // [((i,j), c)])

ppmFromCanvas :: Canvas -> String
ppmFromCanvas c = unlines ("P3" : [show w ++ " " ++ show h ] ++ ["255"] ++ pixelData c)
    where
        w = width c
        h = height c

clamp :: Float -> Int
clamp = round . (255.0*) . max 0.0 . min 1.0

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs =
  let (xs0,xs1) = splitAt n xs
  in  xs0 : group n xs1

pixelData :: Canvas -> [String]
pixelData c = map unwords ( group 15 $ concat [  map ((show . clamp) . ($ p)) [red, green, blue] | p <- allPixels c])

setAllPixelsTo :: Canvas -> Color -> Canvas
setAllPixelsTo (Canvas m) cl = Canvas (m // [ (i, cl) | i <- indices m])
