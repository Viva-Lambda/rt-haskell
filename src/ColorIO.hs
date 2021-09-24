-- input output of colors
module ColorIO where

import GHC.Float
import Vector
import Ray
import Hittable.HittableList
import Hittable.Hittable
import Utils


rayColor :: Ray -> HittableList -> Vector
rayColor Rd {origin = a, direction = b} world =
    let ray = Rd {origin = a, direction = b}
        hrec = HRec {point = VecFromList [0.0, 0.0, 0.0],
                     pnormal = VecFromList [0.0, 0.0, 0.0],
                     dist = 0.0}
        (nhrec, isHit) = hit world ray 0.0 infty hrec
    in if isHit
       then let hnorm = add (pnormal nhrec) (VecFromList [1.0, 1.0, 1.0])
            in multiplyS hnorm 0.5
       else let
                unitDirection = toUnit b
                yval = vget 1 unitDirection
                ntval = (yval + 1.0) * 0.5
                oneMin = 1.0 - ntval
                cval = multiplyS (VecFromList [1.0, 1.0, 1.0]) oneMin
                oval = multiplyS (VecFromList [0.5, 0.7, 1.0]) ntval
            in add cval oval
            -- in error $ "b: " ++ show b ++ " unit " ++ show unitDirection

