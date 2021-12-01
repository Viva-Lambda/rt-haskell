-- hit record module
module Hittable.HitRecord where

import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray
import Material.Material


data HitRecord = HRec {point :: Vector, pnormal :: Vector, hdist :: Double,
                       hUV_u :: Double, hUV_v :: Double,
                       matPtr :: Material, isFront :: Bool}

emptyRecord :: Int -> HitRecord
emptyRecord nbDims = HRec {point = zeroV nbDims,
                           pnormal = zeroV nbDims,
                           hdist = 0.0,
                           hUV_u = 0.0,
                           hUV_v = 0.0,
                           matPtr = NoMat,
                           isFront = False}

emptyRec :: HitRecord
emptyRec = emptyRecord 3

setFaceNormal :: HitRecord -> Ray -> Vector -> HitRecord
setFaceNormal HRec {point = p, 
                    pnormal = pv, 
                    hdist = t,
                    hUV_u = u,
                    hUV_v = v,
                    matPtr = m,
                    isFront = _} Rd {origin = ro, direction = rd} ov =
    let ffaceCond = (dot rd ov ) < 0.0
    in if ffaceCond
       then HRec {point = p, pnormal = ov, hUV_u = u,
                  hUV_v = v, hdist = t, matPtr = m,
                  isFront = ffaceCond}
       else HRec {point = p,
                  pnormal = multiplyS ov (-1.0),
                  hdist = t,
                  matPtr = m,
                  hUV_u = u,
                  hUV_v = v,
                  isFront = ffaceCond
                  }
