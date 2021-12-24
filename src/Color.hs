-- color input output, conversion module
module Color where

import GHC.Float hiding(clamp)
import Math3D.Vector
import Math3D.CommonOps
import Utility.Utils

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v = map double2Int (vec2List v)

writeColor :: Vector -> Int -> String
writeColor clr sample_nb =
    let v = if (all isNaN (vec2List clr))
            then zeroV3
            else clr
        scale = 1.0 / (int2Double sample_nb)
        sv = multiplyS v scale
        svgamma = vecScalarOp sqrt sv
        nsv = clampV svgamma 0.0 0.999
        nv = multiplyS nsv 256.0
        nvints = vecToInt nv
    in unwords $! map show nvints
