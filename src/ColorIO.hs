{-# LANGUAGE BangPatterns #-}
-- input output of colors
module ColorIO where

import GHC.Float
import Vector
import Ray
import Hittable.HittableList
import Hittable.Hittable
import Hittable.HitRecord
import Material.Scatter
import Utility.Utils
import System.Random
import Random
import Prelude hiding(subtract)


rayColor :: RandomGen g => Ray -> HittableList -> Int -> g -> (Vector, g)
rayColor !ray !world !depth !gen =
    if depth <= 0
    then (zeroV3, gen)
    else let hrec = emptyRecord 3
             (hithrec, isHit) = hit world ray 0.001 infty hrec
             HRec{point = recp,
                  pnormal = recnorm,
                  matPtr = m} = hithrec
         in if isHit
            then let sout = scatter gen m ray hithrec
                     (g, natten, outray, isScattering) = sout
                 in if isScattering
                    then let (ncolor, g2) = rayColor outray world (depth-1) g
                         in (multiply ncolor natten, g2)
                    else (zeroV3, g)
            else let unitDirection = toUnit (direction ray)
                     ntval = ((vget unitDirection 1) + 1.0) * 0.5
                     oneMin = 1.0 - ntval
                     cval = VList [oneMin, oneMin, oneMin]
                     oval = multiplyS (VList [0.5, 0.7, 1.0]) ntval
                 in (add cval oval, gen)
