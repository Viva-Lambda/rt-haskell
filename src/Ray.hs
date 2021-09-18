-- ray module
module Ray where

import Vector

data Ray = Rd {
        origin :: Vector,
        direction :: Vector
    } deriving (Show, Eq)

at :: Ray -> Double -> Vector
at (Rd {origin = a, direction = b}) c =
    let cVec = VecFromScalar c (vsize b)
        cMultDir = multiply cVec b
    in add a cMultDir
