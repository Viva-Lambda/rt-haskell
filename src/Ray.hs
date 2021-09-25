-- ray module
module Ray where

import Vector

data Ray = Rd {
        origin :: Vector,
        direction :: Vector
    } deriving (Show, Eq)

zeroRay :: Int -> Ray
zeroRay nbDims = Rd {origin = zeroV nbDims, direction = zeroV nbDims}

at :: Ray -> Double -> Vector
at (Rd {origin = a, direction = b}) c = add a (multiplyS b c)
