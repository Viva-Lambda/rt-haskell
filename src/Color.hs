-- color input output, conversion module
module Color where

import GHC.Float
import Vector

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v =
    let vd = fromScalarToList v
        (VecFromList d) = vd
    in map double2Int d

scaleAndCast :: Vector -> [Int]
scaleAndCast v =
    let vecSize = vsize v
        scaleVec = VecFromScalar 255.9 vecSize
        vScaled = multiply v scaleVec
    in vecToInt vScaled

vecAsColor :: Vector -> String
vecAsColor v = unwords $ map show (scaleAndCast v)
