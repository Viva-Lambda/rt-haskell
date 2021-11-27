{-# LANGUAGE BangPatterns #-}
-- translate object
module Hittable.Translate where

import Hittable.HittableObj
import Hittable.Hittable
import Hittable.HitRecord
import Hittable.Aabb
import Math3D.Vector
import Math3D.CommonOps
import Math3D.Ray
import Prelude hiding(subtract)

data Translatable = Translate {offset :: Vector, tobject :: HittableObj}

instance Hittable Translatable where
    {-# INLINE hit #-}
    hit a !ry !tmin !tmax !hrec =
        let oset = offset a
            transobj = tobject a
            movedRay = Rd {origin = subtract (origin ry) oset,
                           direction = direction ry,
                           rtime = rtime ry}
            (nhrec, isHit) = hit transobj movedRay tmin tmax hrec
        in if not isHit
           then (nhrec, False)
           else let npoint = add (point nhrec) oset
                    norm = pnormal nhrec
                    hr = HRec{
                        point = npoint,
                        pnormal = pnormal nhrec,
                        hdist = hdist nhrec,
                        hUV_u = hUV_u nhrec,
                        hUV_v = hUV_v nhrec,
                        matPtr = matPtr nhrec,
                        isFront = isFront nhrec
                        }
                in (setFaceNormal hr movedRay norm, True)

    boundingBox a t0 t1 ab =
        let transobj = tobject a
            (oab, isBound) = boundingBox transobj t0 t1 ab
        in if not isBound
           then (oab, False)
           else let minb = add (aabbMin oab) (offset a)
                    maxb = add (aabbMax oab) (offset a)
                in (AaBbox {aabbMin = minb, aabbMax = maxb}, True)
