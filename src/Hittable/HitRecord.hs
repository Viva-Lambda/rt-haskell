-- hit record module
module Hittable.HitRecord where

import Vector
import Ray
import Material.Material


data HitRecord = HRec {point :: Vector, pnormal :: Vector, dist :: Double,
                       matPtr :: Material, isFront :: Bool}

emptyRecord :: Int -> HitRecord
emptyRecord nbDims = HRec {point = zeroV nbDims,
                           pnormal = zeroV nbDims,
                           dist = 0.0,
                           matPtr = NoMat,
                           isFront = False}

setFaceNormal :: HitRecord -> Ray -> Vector -> HitRecord
setFaceNormal HRec {point = p, 
                    pnormal = pv, 
                    dist = t,
                    matPtr = m,
                    isFront = _} Rd {origin = ro, direction = rd} ov =
    let ffaceCond = (dot rd ov ) < 0.0
    in if ffaceCond
       then HRec {point = p, pnormal = ov, dist = t, matPtr = m, isFront = ffaceCond}
       else HRec {point = p,
                  pnormal = multiplyS ov (-1.0),
                  dist = t,
                  matPtr = m,
                  isFront = ffaceCond
                  }


