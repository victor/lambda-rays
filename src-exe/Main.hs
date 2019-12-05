{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Projectile
import Quadruple
import Canvas

loopWorld :: World -> Projectile -> [Projectile]
loopWorld wo p' = do
    let p = tick wo p'
    if y (position p) <= 0 then [p]
    else p : loopWorld wo p


main :: IO ()
main = do
    let width = 900
    let height = 550

    let start = point 0 1 0
    let vel = normalize(vector 1  1.8  0) |* 11.25

    let p = Projectile { position = start, velocity = vel}
    let g = vector 0 (-0.1) 0
    let wi = vector (-0.01) 0 0
    let world = World {gravity = g, wind = wi} 
    

    let canvass = foldl (\pixel c -> writePixelAt fst pixel snd pixel c Color(0.8, 0.8, 0.1))  createCanvas width height  [ ((floor (x pos)), (floor (height - (y pos)))) | i <- loopWorld world p, pos <- position i]


    print ppmFromCanvas canvass


