-- color input output, conversion module
module Color where

import GHC.Float hiding(clamp)
import Vector
import Utils

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v =
    let VList d = v
    in map double2Int d

writeColor :: Vector -> Int -> String
writeColor v sample_nb =
    let scale = 1.0 / (int2Double sample_nb)
        sv = multiplyS v scale
        svgamma = vecScalarOp sqrt sv
        nsv = clampV svgamma 0.0 0.999
        nv = multiplyS nsv 256.0
        nvints = vecToInt nv
    in unwords $! map show nvints
