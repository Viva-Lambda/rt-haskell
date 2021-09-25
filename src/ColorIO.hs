-- input output of colors
module ColorIO where

import GHC.Float
import Vector
import Ray
import Hittable.HittableList
import Hittable.Hittable
import Utils
import System.Random
import Random
import Prelude hiding(subtract)


rayColor :: RandomGen g => Ray -> HittableList -> Int -> g -> Vector
rayColor ray world depth gen =
    if depth <= 0
    then VecFromList [0.0, 0.0, 0.0]
    else let hrec = HRec {point = VecFromList [0.0, 0.0, 0.0],
                     pnormal = VecFromList [0.0, 0.0, 0.0],
                     dist = 0.0}
             (HRec{point = recp, 
                   pnormal = recnorm, 
                   dist=_}, isHit) = hit world ray 0.001 infty hrec
         in if isHit
            then let (randv, g) = randomUnitVector gen
                     t2 = add recnorm randv
                     -- (t2, g) = randomHemisphere gen recnorm
                     target = add recp t2
                     nray = Rd {origin = recp, 
                                direction = subtract target recp}
                     ncolor = rayColor nray world (depth-1) g
                 in multiplyS ncolor 0.5
            else let unitDirection = toUnit (direction ray)
                     ntval = ((vget unitDirection 1) + 1.0) * 0.5
                     oneMin = 1.0 - ntval
                     cval = VecFromList [oneMin, oneMin, oneMin]
                     oval = multiplyS (VecFromList [0.5, 0.7, 1.0]) ntval
                 in add cval oval
            -- in error $ "b: " ++ show b ++ " unit " ++ show unitDirection
