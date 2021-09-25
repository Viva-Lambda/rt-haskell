-- hit record module
module Hittable.HitRecord where

import Vector
import Ray
import Material.Material


data HitRecord = HRec {point :: Vector, pnormal :: Vector, dist :: Double,
                       matPtr :: Material}

emptyRecord :: Int -> HitRecord
emptyRecord nbDims = HRec {point = zeroV nbDims,
                           pnormal = zeroV nbDims,
                           dist = 0.0,
                           matPtr = NoMat}

setFaceNormal :: HitRecord -> Ray -> Vector -> HitRecord
setFaceNormal HRec {point = p, 
                    pnormal = pv, 
                    dist = t,
                    matPtr = m} Rd {origin = ro, direction = rd} ov =
    let ffaceCond = (dot rd ov ) < 0.0
    in if ffaceCond
       then HRec {point = p, pnormal = ov, dist = t, matPtr = m}
       else HRec {point = p, 
                  pnormal = multiplyS ov (-1.0), 
                  dist = t, 
                  matPtr = m}


