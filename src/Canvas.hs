module Canvas (
    Color(..)
    , red
    , green
    , blue
) where

-- No point in generalizing the type, Float is more than enough for all the possible colors
data Color = Color (Float, Float, Float) deriving (Show)

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