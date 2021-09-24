-- module for hittable type
module Hittable.Hittable where

import Vector
import Ray

data HitRecord = HRec {point :: Vector, pnormal :: Vector, dist :: Double}

setFaceNormal :: HitRecord -> Ray -> Vector -> HitRecord
setFaceNormal HRec {point = p, 
                    pnormal = pv, 
                    dist = t} Rd {origin = ro, direction = rd} ov =
    let ffaceCond = (dot rd ov ) < 0.0
    in if ffaceCond
       then HRec {point = p, pnormal = ov, dist = t}
       else HRec {point = p, pnormal = multiplyS ov (-1.0), dist = t}

class Hittable a where
    hit :: a -> Ray -> Double -> Double -> HitRecord -> (HitRecord, Bool)

