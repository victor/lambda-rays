module Canvas (
    Color(..)
    , red
    , green
    , blue
    , (*|)
    , (⨯)
) where

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