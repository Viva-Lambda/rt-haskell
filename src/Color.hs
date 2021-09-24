-- color input output, conversion module
module Color where

import GHC.Float hiding(clamp)
import Vector
import Utils

-- print vector
vecToInt :: Vector -> [Int]
vecToInt v =
    let vd = fromScalarToList v
        (VecFromList d) = vd
    in map double2Int d

writeColor :: Vector -> Int -> String
writeColor v sample_nb =
    let r = vget v 0
        g = vget v 1
        b = vget v 2
        scale = 1.0 / (int2Double sample_nb)
        rs = r * scale
        gs = g * scale
        bs = b * scale
        rc = clamp rs 0.0 0.999
        gc = clamp gs 0.0 0.999
        bc = clamp bs 0.0 0.999
        nv = VecFromList [rc * 256, gc * 256, bc * 256]
        nvints = vecToInt nv
    in unwords $ map show nvints
