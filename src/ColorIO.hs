-- input output of colors
module ColorIO where

import GHC.Float
import Vector
import Ray
import Hittable.HittableList
import Hittable.Hittable
import Hittable.HitRecord
import Material.Scatter
import Utils
import System.Random
import Random
import Prelude hiding(subtract)


rayColor :: RandomGen g => Ray -> HittableList -> Int -> g -> Vector
rayColor ray world depth gen =
    if depth <= 0
    then zeroV 3
    else let hrec = emptyRecord 3
             (HRec{point = recp,
                   pnormal = recnorm,
                   dist=_, matPtr = m}, isHit) = hit world ray 0.001 infty hrec
         in if isHit
            then let nray = zeroRay 3
                     atten = zeroV 3
                     sout = scatter gen m ray hrec atten nray
                     (g, natten, outray, isScattering) = sout
                 in if isScattering
                    then let ncolor = rayColor outray world (depth-1) g
                         in multiply ncolor natten
                    else zeroV 3
            else let unitDirection = toUnit (direction ray)
                     ntval = ((vget unitDirection 1) + 1.0) * 0.5
                     oneMin = 1.0 - ntval
                     cval = VList [oneMin, oneMin, oneMin]
                     oval = multiplyS (VList [0.5, 0.7, 1.0]) ntval
                 in add cval oval
            -- in error $ "b: " ++ show b ++ " unit " ++ show unitDirection
